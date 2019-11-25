module GlossToGif where

import Data.Active (Dynamic, Era, Time)
import Graphics.Gloss (Picture)
import Graphics.Gloss.Export.Image (Size)
import qualified Data.Active as Active
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Export.Gif as Export


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
  -> Dynamic Picture
  -> IO ()
renderGif filePath size dynamic = do
  Export.exportPicturesToGif
    (ceiling centisecondsPerFrame)  -- at least 1
    Export.LoopingForever
    size
    Gloss.white
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
