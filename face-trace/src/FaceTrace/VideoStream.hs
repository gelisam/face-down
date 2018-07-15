{-# LANGUAGE TemplateHaskell #-}
module FaceTrace.VideoStream where

import Codec.FFmpeg
import Control.Lens
import Data.Maybe
import Graphics.Gloss.Interface.IO.Animate

import FaceTrace.Frame
import FaceTrace.ReloadableRef
import FaceTrace.Types


data VideoStream = VideoStream
  { videoStreamNextFrameTime :: IO (Maybe (Frame, Timestamp))
  }

videoStreamNextFrame :: VideoStream -> IO (Maybe Frame)
videoStreamNextFrame videoStream = over _Just fst <$> videoStreamNextFrameTime videoStream

withVideoStream :: FilePath
                -> (ReloadableRef VideoStream -> IO a)
                -> IO a
withVideoStream filePath = withReloadableRef' load
  where
    load :: IO (VideoStream, IO ())
    load = do
      (nextFrameTime, unload) <- imageReaderTime (File filePath)
      pure (VideoStream nextFrameTime, unload)


playVideoStream :: String -> VideoStream -> IO ()
playVideoStream windowTitle videoStream = do
  firstFrame <- fromMaybe defaultFrame <$> videoStreamNextFrame videoStream

  let nextFrame :: Float -> IO Picture
      nextFrame _ = maybe mempty framePicture <$> videoStreamNextFrame videoStream

  animateFixedIO (InWindow windowTitle (frameWidth firstFrame, frameHeight firstFrame) (10, 10))
                 black
                 nextFrame
                 mempty
