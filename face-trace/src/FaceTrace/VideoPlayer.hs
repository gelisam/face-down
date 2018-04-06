{-# LANGUAGE TemplateHaskell #-}
module FaceTrace.VideoPlayer (videoPlayer) where

import Control.Lens
import Data.Maybe
import Data.Ratio
import Graphics.Gloss.Interface.IO.Game
import System.Exit

import Control.Monad.Extra
import FaceTrace.Frame
import FaceTrace.Video


data State = State
  { _statePlaying   :: Bool
  , _stateTimestamp :: Double
  }

makeLenses ''State

videoPlayer :: String -> Video -> IO ()
videoPlayer windowTitle video = do
  (pixelWidth, pixelHeight) <- videoDimentions video
  pixelAspectRatio <- videoPixelAspectRatio video
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

      draw :: State -> IO Picture
      draw = view stateTimestamp
         >>> videoGetFrameAtTimestamp video
         >&> maybe blank framePicture
         >>> scale scaleX scaleY

      react :: Event -> State -> IO State
      react (EventKey (Char 'q')            _  _ _) = \_
                                                   -> exitSuccess
      react (EventKey (Char 'r')            _  _ _) = pure
                                                  >&> stateTimestamp .~ 0
      react (EventKey (SpecialKey KeySpace) Up _ _) = pure
                                                  >&> statePlaying %~ not
      react _                                       = pure

      update :: Float -> State -> IO State
      update dt state = pure state
                    <&> if state ^. statePlaying
                        then stateTimestamp +~ realToFrac dt
                        else id

  playIO display
         black
         30
         (State False 0)
         draw
         react
         update
