{-# LANGUAGE TemplateHaskell #-}
module FaceUp.Video where

import Prelude
import Control.Exception
import Control.Lens
import Data.IORef
import Data.Maybe
import Graphics.Gloss.Interface.IO.Game
import System.Exit

import Control.Monad.Extra
import FaceUp.Frame
import FaceUp.VideoStream


data VideoState = VideoState
  { _videoStateStream      :: VideoStream
  , _videoStateFrameNumber :: Int
  }

makeLenses ''VideoState

-- | Must be released with 'videoStateClose'
videoStateOpen :: FilePath -> IO VideoState
videoStateOpen filePath = do
  videoStream <- videoStreamOpen filePath
  pure $ VideoState videoStream 0

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


videoNextFrame :: Video -> IO (Maybe Frame)
videoNextFrame video = do
  videoState <- readIORef (videoStateRef video)
  frame <- videoStreamNextFrame (view videoStateStream videoState)
  let videoState' = videoState
                  & videoStateFrameNumber +~ 1
  writeIORef (videoStateRef video) videoState'
  pure frame

videoCurrentFrameNumber :: Video -> IO Int
videoCurrentFrameNumber = fmap (view videoStateFrameNumber) . readIORef . videoStateRef

videoGetFrame :: Video -> Int -> IO (Maybe Frame)
videoGetFrame video frameNumber | frameNumber < 1 = pure Nothing
                                | otherwise       = do
  maybeFrame <- videoNextFrame video
  currentFrameNumber <- videoCurrentFrameNumber video
  case maybeFrame of
    Just _  | currentFrameNumber <  frameNumber -> videoGetFrame video frameNumber
    Nothing | currentFrameNumber <  frameNumber -> videoGetFrame video frameNumber
    _       | currentFrameNumber == frameNumber -> pure maybeFrame
    _                         {- > -}           -> do videoReset video
                                                      videoGetFrame video frameNumber


videoPlayer :: String -> Video -> IO ()
videoPlayer windowTitle video = do
  firstFrame <- fromMaybe defaultFrame <$> videoNextFrame video

  let display :: Display
      display = InWindow windowTitle
                         (frameWidth firstFrame, frameHeight firstFrame)
                         (10, 10)

      draw :: Int -> IO Picture
      draw = videoGetFrame video
         >&> fromMaybe defaultFrame
         >&> framePicture

      react :: Event -> Int -> IO Int
      react (EventKey (Char 'q') _ _ _) _           = exitSuccess
      react (EventKey (Char 'r') _ _ _) _           = pure 0
      react _                           frameNumber = pure frameNumber

      update :: Float -> Int -> IO Int
      update _ = (+1)
             >>> pure

  playIO display
         black
         30
         0
         draw
         react
         update
