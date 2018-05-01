{-# LANGUAGE LambdaCase, TemplateHaskell #-}
module FaceTrace.VideoLoader where

import Control.Applicative
import Control.Concurrent.Async
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Concurrent.STM
import Data.Map.Strict (Map)
import Data.Maybe
import qualified Data.Map.Strict as Map

import FaceTrace.Frame
import FaceTrace.VideoStream


data VideoLoader = VideoLoader
  { _videoLoaderFirstFrameDropped :: TVar Bool
  , _videoLoaderLastFrameLoaded   :: TVar Bool
  , _videoLoaderLoadedFrames      :: TVar (Map Double Frame)
  , _videoLoaderPlayTime          :: TVar Double
  , _videoLoaderLoadingThread     :: Async ()
  , _videoLoaderTrailingThread    :: Async ()
  , _videoLoaderVideoStream       :: TMVar VideoStream
  }

makeLenses ''VideoLoader


-- | Must be released with 'videoLoaderClose'.
videoLoaderOpen :: FilePath -> Double -> Double -> IO VideoLoader
videoLoaderOpen filePath trailingDuration preloadDuration = do
  firstFrameDroppedTVar <- atomically $ newTVar False
  lastFrameLoadedTVar   <- atomically $ newTVar False
  loadedFramesTVar      <- atomically $ newTVar mempty
  playTimeTVar          <- atomically $ newTVar 0
  videoStreamTMVar <- do
    videoStream <- videoStreamOpen filePath
    atomically $ newTMVar videoStream

  let firstLoadedFrameTimestamp :: STM (Maybe Double)
      firstLoadedFrameTimestamp = do
        loadedFrames <- readTVar loadedFramesTVar
        case Map.lookupMin loadedFrames of
          Nothing -> pure Nothing
          Just (firstTimestamp, _) -> pure $ Just firstTimestamp

  let secondLoadedFrameTimestamp :: STM (Maybe Double)
      secondLoadedFrameTimestamp = do
        loadedFrames <- readTVar loadedFramesTVar
        firstLoadedFrameTimestamp >>= \case
          Nothing -> pure Nothing
          Just firstTimestamp -> do
            case Map.lookupGT firstTimestamp loadedFrames of
              Nothing -> pure Nothing
              Just (secondTimestamp, _) -> pure $ Just secondTimestamp

  let lastLoadedFrameTimestamp :: STM (Maybe Double)
      lastLoadedFrameTimestamp = do
        loadedFrames <- readTVar loadedFramesTVar
        pure $ fst <$> Map.lookupMax loadedFrames

  let isPlayFrameLoading :: STM Bool
      isPlayFrameLoading = isNothing
                       <$> getPlayFrameInternal firstFrameDroppedTVar
                                                lastFrameLoadedTVar
                                                loadedFramesTVar
                                                playTimeTVar

  let blockUntilLoadFrame :: STM ()
      blockUntilLoadFrame = do
        playFrameIsLoading <- isPlayFrameLoading
        if playFrameIsLoading
        then pure ()
        else do
          lastFrameLoaded <- readTVar lastFrameLoadedTVar
          guard (not lastFrameLoaded)

          preloadTime <- fromMaybe (-infinity) <$> lastLoadedFrameTimestamp
          playTime <- readTVar playTimeTVar
          guard (preloadTime < playTime + preloadDuration)

  let blockUntilDropFrame :: STM ()
      blockUntilDropFrame = secondLoadedFrameTimestamp >>= \case
        Nothing -> retry
        Just secondTimestamp -> do
          playTime <- readTVar playTimeTVar
          guard (secondTimestamp <= playTime - trailingDuration)

  let blockUntilReset :: STM ()
      blockUntilReset = do
        firstFrameDropped <- readTVar firstFrameDroppedTVar
        guard firstFrameDropped

        playTime <- readTVar playTimeTVar
        firstLoadedFrameTimestamp >>= \case
          Just trailTime | playTime < trailTime -> pure ()
          _ -> retry

  -- True for reset
  let blockUntilResetOrLoadFrame :: STM Bool
      blockUntilResetOrLoadFrame = (True  <$ blockUntilReset)
                               <|> (False <$ blockUntilLoadFrame)

  -- try to keep the last loaded frame >= playTime + preloadDuration,
  -- but reset whenever playTime < last dropped frame
  loadingThread <- async $ forever $ do
    -- block until we need to load the next frame or reset
    (shouldReset, videoStream) <- atomically $ do
      (,) <$> blockUntilResetOrLoadFrame
          <*> takeTMVar videoStreamTMVar

    -- load the next frame or reset
    if shouldReset
    then do
      videoStreamClose videoStream
      videoStream' <- videoStreamOpen filePath
      atomically $ do
        writeTVar  firstFrameDroppedTVar False
        writeTVar  lastFrameLoadedTVar   False
        writeTVar  loadedFramesTVar      mempty
        putTMVar   videoStreamTMVar      videoStream'
    else do
      maybeFrameTime <- videoStreamNextFrameTime videoStream
      atomically $ do
        putTMVar videoStreamTMVar videoStream
        case maybeFrameTime of
          Just (frame, time) -> modifyTVar loadedFramesTVar
                              $ Map.insert time frame
          Nothing -> writeTVar lastFrameLoadedTVar True

  -- drop frames older than playTime - trailingDuration
  trailingThread <- async $ forever $ atomically $ do
    -- block until we need to drop the oldest frame
    blockUntilDropFrame

    -- drop the oldest frame
    writeTVar  firstFrameDroppedTVar True
    modifyTVar loadedFramesTVar      Map.deleteMin

  pure $ VideoLoader firstFrameDroppedTVar
                     lastFrameLoadedTVar
                     loadedFramesTVar
                     playTimeTVar
                     loadingThread
                     trailingThread
                     videoStreamTMVar
  where
    infinity :: Double
    infinity = 1/0

videoLoaderClose :: VideoLoader -> IO ()
videoLoaderClose videoLoader = do
  videoStream <- atomically $ takeTMVar (view videoLoaderVideoStream videoLoader)
  videoStreamClose videoStream

  cancel (view videoLoaderLoadingThread   videoLoader)
  cancel (view videoLoaderTrailingThread  videoLoader)

withVideoLoader :: FilePath -> Double -> Double -> (VideoLoader -> IO a) -> IO a
withVideoLoader filePath trailingDuration preloadDuration
  = bracket (videoLoaderOpen filePath trailingDuration preloadDuration)
            videoLoaderClose


setPlayTime :: VideoLoader -> Double -> STM ()
setPlayTime videoLoader t = writeTVar (view videoLoaderPlayTime videoLoader) t

-- | A version of 'getPlayFrame' which only require part of a 'VideoLoader'.
getPlayFrameInternal :: TVar Bool
                     -> TVar Bool
                     -> TVar (Map Double Frame)
                     -> TVar Double
                     -> STM (Maybe (Maybe Frame))
getPlayFrameInternal firstFrameDroppedTVar
                     lastFrameLoadedTVar
                     loadedFramesTVar
                     playTimeTVar = do
  firstFrameDropped <- readTVar firstFrameDroppedTVar
  lastFrameLoaded   <- readTVar lastFrameLoadedTVar
  loadedFrames      <- readTVar loadedFramesTVar
  playTime          <- readTVar playTimeTVar

  let maybePreviousFrame = snd <$> Map.lookupLE playTime loadedFrames
  let maybeNextFrame     = snd <$> Map.lookupGE playTime loadedFrames
  case (maybePreviousFrame, maybeNextFrame) of
    (Just frame, Just _)
      -> pure $ Just $ Just frame
    (Nothing, Just frame) | not firstFrameDropped
      -> pure $ Just $ Just frame
    (Just _, Nothing) | lastFrameLoaded
      -> pure $ Just Nothing
    _ -> pure Nothing

-- | @Nothing@ if we don't have enough information yet. Will eventually return
-- @Just@. @Just Nothing@ if playTime is after the video's last frame.
getPlayFrame :: VideoLoader -> STM (Maybe (Maybe Frame))
getPlayFrame videoLoader = do
  getPlayFrameInternal (view videoLoaderFirstFrameDropped videoLoader)
                       (view videoLoaderLastFrameLoaded   videoLoader)
                       (view videoLoaderLoadedFrames      videoLoader)
                       (view videoLoaderPlayTime          videoLoader)