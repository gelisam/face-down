module FaceUp.VideoStream where

import Codec.FFmpeg
import Control.Exception
import Control.Lens
import Data.Maybe
import Graphics.Gloss.Interface.IO.Animate

import FaceUp.Frame


data VideoStream = VideoStream
  { videoStreamNextFrameTime :: IO (Maybe (Frame, Double))
  , videoStreamClose         :: IO ()
  }

videoStreamNextFrame :: VideoStream -> IO (Maybe Frame)
videoStreamNextFrame videoStream = over _Just fst <$> videoStreamNextFrameTime videoStream

-- | Must be released with 'videoStreamClose'
videoStreamOpen :: FilePath -> IO VideoStream
videoStreamOpen = fmap (uncurry VideoStream)
                . imageReaderTime
                . File

withVideoStream :: FilePath
                -> (VideoStream -> IO a)
                -> IO a
withVideoStream filePath = bracket (videoStreamOpen filePath) videoStreamClose


playVideoStream :: String -> VideoStream -> IO ()
playVideoStream windowTitle videoStream = do
  firstFrame <- fromMaybe defaultFrame <$> videoStreamNextFrame videoStream

  let nextFrame :: Float -> IO Picture
      nextFrame _ = maybe mempty framePicture <$> videoStreamNextFrame videoStream

  animateFixedIO (InWindow windowTitle (frameWidth firstFrame, frameHeight firstFrame) (10, 10))
                 black
                 nextFrame
                 mempty
