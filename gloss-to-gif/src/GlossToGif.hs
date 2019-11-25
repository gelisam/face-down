{-# LANGUAGE ScopedTypeVariables #-}
module GlossToGif where

import Data.Active (Dynamic, Era, Time)
import Graphics.Gloss (Color, Picture)
import Graphics.Gloss.Export.Gif
import Graphics.Gloss.Export.Image
import qualified Data.Active as Active


timestamps
  :: Dynamic a -> Dynamic (Time Rational)
timestamps dynamic
  = Active.mkDynamic (Active.start era) (Active.end era) id
  where
    era :: Era Rational
    era = Active.era dynamic

renderGif
  :: FilePath
  -> Size
  -> GifLooping
  -> Color
  -> Dynamic Picture
  -> IO ()
renderGif filePath size gifLooping bg dynamic = do
  exportPicturesToGif
    (ceiling centisecondsPerFrame)  -- at least 1
    gifLooping
    size
    bg
    filePath
    animation
    timestampsToEvaluate
  where
    fps :: Rational
    fps = 25

    centisecondsPerFrame :: Rational
    centisecondsPerFrame
      = 100 / fps

    animation :: Float -> Picture
    animation
      = Active.runDynamic dynamic
      . realToFrac

    timestampsToEvaluate :: [Float]
    timestampsToEvaluate
      = Active.simulate fps
      . Active.fromDynamic
      . fmap realToFrac
      . timestamps
      $ dynamic
