module GlossToGif where

import Data.Active (Active, Dynamic)
import Graphics.Gloss (Color, Picture)
import Graphics.Gloss.Export.Image (Size)
import qualified Data.Active as Active
import qualified Graphics.Gloss.Export.Gif as Export

import qualified Data.Active.Extra as Active


writeGif
  :: FilePath
  -> Size
  -> Color  -- ^ background color
  -> Int  -- ^ frames per second
  -> Active Picture
  -> IO ()
writeGif filePath size bg fps = do
  Active.onActive (writeImage filePath size bg)
                  (writeLoop  filePath size bg fps)

writeImage
  :: FilePath
  -> Size
  -> Color  -- ^ background color
  -> Picture
  -> IO ()
writeImage filePath size bg picture = do
  Export.exportPicturesToGif
    0
    Export.LoopingNever
    size
    bg
    filePath
    (const picture)
    [0]

writeLoop
  :: FilePath
  -> Size
  -> Color  -- ^ background color
  -> Int  -- ^ frames per second
  -> Dynamic Picture
  -> IO ()
writeLoop filePath size bg fps dynamic = do
  Export.exportPicturesToGif
    (ceiling centisecondsPerFrame)  -- at least 1
    Export.LoopingForever
    size
    bg
    filePath
    animation
    timestampsToEvaluate
  where
    centisecondsPerFrame :: Rational
    centisecondsPerFrame
      = 100 / fromIntegral fps

    animation :: Float -> Picture
    animation
      = Active.runDynamic dynamic
      . realToFrac

    timestampsToEvaluate :: [Float]
    timestampsToEvaluate
      = Active.simulate (fromIntegral fps)
      . Active.fromDynamic
      . fmap realToFrac
      . Active.timestamps
      $ dynamic
