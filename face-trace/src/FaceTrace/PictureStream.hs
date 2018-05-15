module FaceTrace.PictureStream where

import Control.Exception
import Control.Lens
import Data.Maybe
import Graphics.Gloss.Interface.IO.Animate

import FaceTrace.Frame
import FaceTrace.Types
import FaceTrace.VideoInfo
import FaceTrace.VideoStream


data PictureStream = PictureStream
  { pictureStreamWidth         :: Int
  , pictureStreamHeight        :: Int
  , pictureStreamNextFrameTime :: IO (Maybe (Picture, Timestamp))
  , pictureStreamClose         :: IO ()
  }


-- | Must be released with 'pictureStreamClose'
pictureStreamOpen :: FilePath -> IO PictureStream
pictureStreamOpen filePath = do
  (pixelWidth, pixelHeight) <- videoDimentions filePath
  pixelAspectRatio <- videoPixelAspectRatio filePath -- width:height
                  <&> fromMaybe 1
  videoStream <- videoStreamOpen filePath

  -- stretch the image up, not down
  let (displayWidth, displayHeight) =
        if pixelAspectRatio > 0
        then (round (pixelAspectRatio * fromIntegral pixelWidth), pixelHeight)
        else (pixelWidth, round (fromIntegral pixelHeight / pixelAspectRatio))

  let scaleX :: Float
      scaleX = fromIntegral displayWidth / fromIntegral pixelWidth

  let scaleY :: Float
      scaleY = fromIntegral displayHeight / fromIntegral pixelHeight

  let nextFrameTime :: IO (Maybe (Picture, Timestamp))
      nextFrameTime = over (_Just . _1)
                           (scale scaleX scaleY . framePicture)
                  <$> videoStreamNextFrameTime videoStream

  pure $ PictureStream displayWidth
                       displayHeight
                       nextFrameTime
                       (videoStreamClose videoStream)

withPictureStream :: FilePath
                  -> (PictureStream -> IO a)
                  -> IO a
withPictureStream filePath = bracket (pictureStreamOpen filePath)
                                     pictureStreamClose


pictureStreamNextFrame :: PictureStream -> IO (Maybe Picture)
pictureStreamNextFrame pictureStream = over _Just fst
                                   <$> pictureStreamNextFrameTime pictureStream


playPictureStream :: String -> PictureStream -> IO ()
playPictureStream windowTitle pictureStream = do
  let nextFrame :: Float -> IO Picture
      nextFrame _ = fromMaybe mempty <$> pictureStreamNextFrame pictureStream

  let width  = pictureStreamWidth  pictureStream
  let height = pictureStreamHeight pictureStream
  animateFixedIO (InWindow windowTitle (width, height) (10, 10))
                 black
                 nextFrame
                 mempty
