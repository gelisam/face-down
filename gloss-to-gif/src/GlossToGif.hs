module GlossToGif where

import Data.Active (Dynamic)
import Graphics.Gloss (Picture)
import Graphics.Gloss.Export.Image (Size)
import qualified Data.Active as Active
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Export.Gif as Export

import qualified Data.Active.Extra as Active


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
      . Active.timestamps
      $ dynamic
