{-# LANGUAGE DeriveFunctor, TemplateHaskell #-}
module FaceTrace.Stream where

import Codec.FFmpeg
import Control.Lens
import Data.Maybe
import Graphics.Gloss.Interface.IO.Animate
import Linear

import Control.Lens.SetterM
import Control.Monad.Extra
import FaceTrace.Frame
import FaceTrace.ReloadableRef
import FaceTrace.Size
import FaceTrace.Types
import FaceTrace.VideoInfo


data Stream a = Stream
  { _streamSize          :: Size
  , _streamReloadableRef :: ReloadableRef (IO (Maybe (a, Timestamp)))
  }
  deriving Functor

makeLenses ''Stream

streamWidth :: Lens' (Stream a) Float
streamWidth = streamSize . sizeWidth

streamHeight :: Lens' (Stream a) Float
streamHeight = streamSize . sizeHeight

streamFrame :: SetterM IO (Stream a) (Stream b) a b
streamFrame = (streamReloadableRef . unSetterM reloadableRefValue) `thenSetterM` monadicSetter `setterMThen` (_Just . _1)


nextFrameTime :: Stream a -> IO (Maybe (a, Timestamp))
nextFrameTime stream = do
  action <- readReloadableRef (stream ^. streamReloadableRef)
  action

nextFrame :: Stream a -> IO (Maybe a)
nextFrame stream = nextFrameTime stream
                 & mapped . _Just %~ fst

reloadStream :: Stream a -> IO ()
reloadStream = reloadReloadableRef . view streamReloadableRef


-- The raw, unscaled frames
withFrameStream :: FilePath -> (Stream Frame -> IO a) -> IO a
withFrameStream filePath body = do
  (pixelWidth, pixelHeight) <- videoDimentions filePath
  let size :: Size
      size = Size $ V2 (fromIntegral pixelWidth)
                       (fromIntegral pixelHeight)
  withReloadableRef' (imageReaderTime (File filePath)) $ \reloadableRef -> do
    body $ Stream size reloadableRef

-- Unlike 'withFrameStream', this function honors the pixel aspect ratio
withPictureStream :: FilePath -> (Stream Picture -> IO a) -> IO a
withPictureStream filePath body = do
  pixelAspectRatio <- videoPixelAspectRatio filePath -- width:height
                  <&> fromMaybe 1
  withFrameStream filePath $ \frameStream -> do
    let pixelWidth, pixelHeight :: Float
        pixelWidth  = view streamWidth  frameStream
        pixelHeight = view streamHeight frameStream

    -- stretch the image up, not down
    let displayWidth, displayHeight :: Float
        (displayWidth, displayHeight) =
          if pixelAspectRatio > 0
          then (realToFrac pixelAspectRatio * pixelWidth, pixelHeight)
          else (pixelWidth, pixelHeight / realToFrac pixelAspectRatio)

    let scaleX, scaleY :: Float
        scaleX = displayWidth / pixelWidth
        scaleY = displayHeight / pixelHeight

    frameStream & toPictureStream
                & scalePictureStream scaleX scaleY
                & body


toPictureStream :: Stream Frame -> Stream Picture
toPictureStream = over (unSetterM streamFrame) framePicture


scalePictureStream :: Float -> Float -> Stream Picture -> Stream Picture
scalePictureStream scaleX scaleY = streamWidth           %~ (* scaleX)
                               >>> streamHeight          %~ (* scaleY)
                               >>> unSetterM streamFrame %~ scale scaleX scaleY


playPictureStream :: String -> Stream Picture -> IO ()
playPictureStream windowTitle pictureStream = do
  let width  = pictureStream ^. streamWidth  & round
      height = pictureStream ^. streamHeight & round
  animateFixedIO (InWindow windowTitle (width, height) (10, 10))
                 black
                 (\_ -> fromMaybe mempty <$> nextFrame pictureStream)
                 mempty
