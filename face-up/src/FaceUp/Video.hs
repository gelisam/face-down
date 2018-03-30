{-# LANGUAGE RecordWildCards #-}
module FaceUp.Video where

import Control.Exception
import Data.IORef
import Data.Maybe
import Graphics.Gloss.Interface.IO.Animate

import FaceUp.Frame
import FaceUp.VideoStream


data VideoState = VideoState
  { videoStateStream      :: VideoStream
  , videoStateFrameNumber :: Int
  }

-- | Must be released with 'videoStateClose'
videoStateOpen :: FilePath -> IO VideoState
videoStateOpen filePath = do
  videoStream <- videoStreamOpen filePath
  pure $ VideoState videoStream 0

videoStateClose :: VideoState -> IO ()
videoStateClose = videoStreamClose . videoStateStream


data Video = Video
  { videoFilePath :: FilePath
  , videoStateRef :: IORef VideoState
  }

-- | Must be released with 'videoClose'
videoOpen :: FilePath -> IO Video
videoOpen filePath = do
  videoState <- videoStateOpen filePath
  ref <- newIORef videoState
  pure $ Video filePath ref

videoClose :: Video -> IO ()
videoClose video = do
  videoState <- readIORef (videoStateRef video)
  videoStateClose videoState

videoReset :: Video -> IO ()
videoReset video = do
  videoState <- readIORef (videoStateRef video)
  videoStateClose videoState

  videoState' <- videoStateOpen (videoFilePath video)
  writeIORef (videoStateRef video) videoState'

withVideo :: FilePath -> (Video -> IO a) -> IO a
withVideo filePath = bracket (videoOpen filePath) videoClose


videoNextFrame :: Video -> IO (Maybe Frame)
videoNextFrame video = do
  videoState <- readIORef (videoStateRef video)
  videoStreamNextFrame (videoStateStream videoState)

videoGetFrameNumber :: Video -> IO Int
videoGetFrameNumber = fmap videoStateFrameNumber . readIORef . videoStateRef


playVideo :: String -> Video -> IO ()
playVideo windowTitle video = do
  firstFrame <- fromMaybe defaultFrame <$> videoNextFrame video

  let nextFrame :: Float -> IO Picture
      nextFrame _ = maybe mempty framePicture <$> videoNextFrame video

  animateFixedIO (InWindow windowTitle (frameWidth firstFrame, frameHeight firstFrame) (10, 10))
                 black
                 nextFrame
                 mempty
