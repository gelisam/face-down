{-# LANGUAGE LambdaCase, TemplateHaskell #-}
module FaceTrace.Video where

import Codec.FFmpeg.Probe
import Control.Exception
import Control.Lens
import Data.IORef
import Data.Ratio

import FaceTrace.Frame
import FaceTrace.VideoStream


data VideoState = VideoState
  { _videoStateStream      :: VideoStream
  , _videoStateFrameNumber :: Int
  , _videoStateTimestamp   :: Double  -- seconds
  }

makeLenses ''VideoState

-- | Must be released with 'videoStateClose'
videoStateOpen :: FilePath -> IO VideoState
videoStateOpen filePath = do
  videoStream <- videoStreamOpen filePath
  pure $ VideoState videoStream 0 0

videoStateClose :: VideoState -> IO ()
videoStateClose = videoStreamClose . view videoStateStream


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


-- | (width, height)
videoDimentions :: Video -> IO (Int, Int)
videoDimentions video = do
  withAvFile (videoFilePath video) $ do
    withStream 0 $ do
      Just avCodecContext <- codecContext
      streamImageSize avCodecContext

-- | width:height
videoPixelAspectRatio :: Video -> IO (Maybe (Ratio Int))
videoPixelAspectRatio video = do
  withAvFile (videoFilePath video) $ do
    withStream 0 $ do
      Just avCodecContext <- codecContext
      streamSampleAspectRatio avCodecContext

videoNextFrame :: Video -> IO (Maybe Frame)
videoNextFrame video = do
  videoState <- readIORef (videoStateRef video)
  videoStreamNextFrameTime (view videoStateStream videoState) >>= \case
    Nothing -> pure Nothing
    Just (frame, timestamp) -> do
      let videoState' = videoState
                      & videoStateFrameNumber +~ 1
                      & videoStateTimestamp   .~ timestamp
      writeIORef (videoStateRef video) videoState'
      pure . Just $ frame

videoCurrentFrameNumber :: Video -> IO Int
videoCurrentFrameNumber = fmap (view videoStateFrameNumber) . readIORef . videoStateRef

videoCurrentTimestamp :: Video -> IO Double
videoCurrentTimestamp = fmap (view videoStateTimestamp) . readIORef . videoStateRef

videoGetFrame :: Video -> Int -> IO (Maybe Frame)
videoGetFrame video frameNumber = do
  maybeFrame <- videoNextFrame video
  currentFrameNumber <- videoCurrentFrameNumber video
  case maybeFrame of
    Just _  | currentFrameNumber <  frameNumber -> videoGetFrame video frameNumber
    Nothing | currentFrameNumber <  frameNumber -> pure Nothing
    _       | currentFrameNumber == frameNumber -> pure maybeFrame
    _                         {- > -}           -> do
      videoReset video
      maybeFirstFrame <- videoNextFrame video
      firstFrameNumber <- videoCurrentFrameNumber video
      if frameNumber <= firstFrameNumber
      then pure maybeFirstFrame
      else videoGetFrame video frameNumber

videoGetFrameAtTimestamp :: Video -> Double -> IO (Maybe Frame)
videoGetFrameAtTimestamp video timestamp = do
  previousTimestamp <- videoCurrentTimestamp video
  maybeFrame <- videoNextFrame video
  currentTimestamp <- videoCurrentTimestamp video
  case maybeFrame of
    Just _  | currentTimestamp < timestamp -> videoGetFrameAtTimestamp video timestamp
    Nothing | currentTimestamp < timestamp -> pure Nothing
    _       | previousTimestamp < timestamp
           && timestamp <= currentTimestamp -> pure maybeFrame
    _                                       -> do
      videoReset video
      maybeFirstFrame <- videoNextFrame video
      firstTimestamp <- videoCurrentTimestamp video
      if timestamp <= firstTimestamp
      then pure maybeFirstFrame
      else videoGetFrameAtTimestamp video timestamp
