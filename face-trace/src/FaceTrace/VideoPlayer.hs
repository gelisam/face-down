module FaceTrace.VideoPlayer where

import Control.Lens
import Data.Maybe
import Data.Ratio
import Graphics.Gloss.Interface.IO.Game
import System.Exit

import Control.Monad.Extra
import FaceTrace.Frame
import FaceTrace.Video


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

      draw :: Double -> IO Picture
      draw = videoGetFrameAtTimestamp video
         >&> maybe blank framePicture
         >&> scale scaleX scaleY

      react :: Event -> Double -> IO Double
      react (EventKey (Char 'q') _ _ _) _         = exitSuccess
      react (EventKey (Char 'r') _ _ _) _         = pure 0
      react _                           timestamp = pure timestamp

      update :: Float -> Double -> IO Double
      update dt = (+ realToFrac dt)
              >>> pure

  playIO display
         black
         30
         0
         draw
         react
         update
