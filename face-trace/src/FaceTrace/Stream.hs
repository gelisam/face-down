{-# LANGUAGE DeriveFunctor, TemplateHaskell #-}
module FaceTrace.Stream where

import Codec.FFmpeg
import Control.Lens
import Data.Maybe
import Graphics.Gloss.Interface.IO.Animate

import Control.Lens.SetterM
import Control.Monad.Extra
import FaceTrace.Frame
import FaceTrace.ReloadableRef
import FaceTrace.Types
import FaceTrace.VideoInfo


data Stream a = Stream
  { _streamWidth         :: Int
  , _streamHeight        :: Int
  , _streamReloadableRef :: ReloadableRef (IO (Maybe (a, Timestamp)))
  }
  deriving Functor

makeLenses ''Stream

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
  withReloadableRef' (imageReaderTime (File filePath)) $ \reloadableRef -> do
    body $ Stream pixelWidth pixelHeight reloadableRef

-- Unlike 'withFrameStream', this function honors the pixel aspect ratio
withPictureStream :: FilePath -> (Stream Picture -> IO a) -> IO a
withPictureStream filePath body = do
  pixelAspectRatio <- videoPixelAspectRatio filePath -- width:height
                  <&> fromMaybe 1
  withFrameStream filePath $ \frameStream -> do
    let pixelWidth, pixelHeight :: Int
        pixelWidth  = view streamWidth  frameStream
        pixelHeight = view streamHeight frameStream

    -- stretch the image up, not down
    let displayWidth, displayHeight :: Int
        (displayWidth, displayHeight) =
          if pixelAspectRatio > 0
          then (round (pixelAspectRatio * fromIntegral pixelWidth), pixelHeight)
          else (pixelWidth, round (fromIntegral pixelHeight / pixelAspectRatio))

    let scaleX, scaleY :: Float
        scaleX = fromIntegral displayWidth / fromIntegral pixelWidth
        scaleY = fromIntegral displayHeight / fromIntegral pixelHeight

    frameStream & toPictureStream
                & scalePictureStream scaleX scaleY
                & body


toPictureStream :: Stream Frame -> Stream Picture
toPictureStream = over (unSetterM streamFrame) framePicture


scalePictureStream :: Float -> Float -> Stream Picture -> Stream Picture
scalePictureStream scaleX scaleY = streamWidth           %~ scaleInt scaleX
                               >>> streamHeight          %~ scaleInt scaleY
                               >>> unSetterM streamFrame %~ scale scaleX scaleY
  where
    scaleInt :: Float -> Int -> Int
    scaleInt factor = round . (* factor) . fromIntegral


playPictureStream :: String -> Stream Picture -> IO ()
playPictureStream windowTitle pictureStream = do
  let width  = view streamWidth  pictureStream
      height = view streamHeight pictureStream
  animateFixedIO (InWindow windowTitle (width, height) (10, 10))
                 black
                 (\_ -> fromMaybe mempty <$> nextFrame pictureStream)
                 mempty
