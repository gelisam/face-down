{-# LANGUAGE LambdaCase, TemplateHaskell #-}
module FaceTrace.VideoPlayer (videoPlayer) where

import Control.Concurrent.STM
import Control.Lens
import Data.Maybe
import Data.Ratio
import Graphics.Gloss.Interface.IO.Game
import System.Exit

import Control.Monad.Extra
import FaceTrace.Frame
import FaceTrace.VideoInfo
import FaceTrace.VideoLoader


data State = State
  { _statePlaying   :: Bool
  , _stateTimestamp :: Double
  }

makeLenses ''State


videoPlayer :: String -> FilePath -> IO ()
videoPlayer windowTitle filePath = do
  videoLoader <- videoLoaderOpen filePath 0 1
  (pixelWidth, pixelHeight) <- videoDimentions filePath
  pixelAspectRatio <- videoPixelAspectRatio filePath
                  <&> fromMaybe 1
  let displayAspectRatio :: Ratio Int
      displayAspectRatio = (pixelWidth  * numerator pixelAspectRatio)
                         % (pixelHeight * denominator pixelAspectRatio)

      displayWidth :: Int
      displayWidth = div pixelWidth <$> [1..]
                   & dropWhile (> 1024)
                   & head

      displayHeight :: Int
      displayHeight = round (fromIntegral displayWidth / displayAspectRatio)

      display :: Display
      display = InWindow windowTitle
                         (displayWidth, displayHeight)
                         (10, 10)

      scaleX :: Float
      scaleX = fromIntegral displayWidth / fromIntegral pixelWidth

      scaleY :: Float
      scaleY = fromIntegral displayHeight / fromIntegral pixelHeight

      textPicture :: String -> Picture
      textPicture = translate (fromIntegral (-displayWidth) / 2 + 5)
                              (fromIntegral displayHeight   / 2 - 5)
                  . scale 0.15 0.15
                  . translate 0 (-113)
                  . color white
                  . text

      draw :: State -> IO Picture
      draw _ = atomically (getPlayFrame videoLoader) <&> \case
        Nothing -> textPicture "Loading..."
        Just Nothing -> textPicture "Done!"
        Just (Just frame) -> frame
                           & framePicture
                           & scale scaleX scaleY

      react :: Event -> State -> IO State
      react (EventKey (Char 'q')            _  _ _) = \_ -> do
        videoLoaderClose videoLoader
        exitSuccess
      react (EventKey (Char 'r')            _  _ _) = \state -> do
        atomically $ setPlayTime videoLoader 0
        pure $ state
             & stateTimestamp .~ 0
      react (EventKey (SpecialKey KeySpace) Up _ _) = pure
                                                  >&> statePlaying %~ not
      react _                                       = pure

      update :: Float -> State -> IO State
      update dt state = do
        if state ^. statePlaying
        then do
          atomically (getPlayFrame videoLoader) >>= \case
            Nothing -> pure state
            Just _ -> do
              let t' = state ^. stateTimestamp + realToFrac dt
              atomically $ setPlayTime videoLoader t'
              pure $ state
                   & stateTimestamp .~ t'
        else pure state

  playIO display
         black
         30
         (State False 0)
         draw
         react
         update
