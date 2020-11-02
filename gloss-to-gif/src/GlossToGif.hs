module GlossToGif where

import Codec.Picture.Types (DynamicImage)
import Data.Active (Active, Dynamic)
import Graphics.Gloss (Color, Picture)
import Graphics.Gloss.Export.Gif (GifDelay)
import Graphics.Gloss.Export.Image (Size)
import qualified Codec.Picture.Gif as JuicyPixels
import qualified Data.Active as Active
import qualified Data.ByteString as ByteString
import qualified Graphics.Gloss.Export.Gif as GlossExport

import qualified Data.Active.Extra as Active


readGif
  :: FilePath
  -> IO ([DynamicImage], [GifDelay])
readGif filePath = do
  bytes <- ByteString.readFile filePath
  case (,) <$> JuicyPixels.decodeGifImages bytes
           <*> JuicyPixels.getDelaysGifImages bytes of
    Left e -> error e
    Right r -> pure r

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
  GlossExport.exportPicturesToGif
    0
    GlossExport.LoopingNever
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
  GlossExport.exportPicturesToGif
    (ceiling centisecondsPerFrame)  -- at least 1
    GlossExport.LoopingForever
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
