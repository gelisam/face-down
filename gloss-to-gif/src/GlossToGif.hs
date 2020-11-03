module GlossToGif where

import Codec.Picture (DynamicImage, Image, Pixel)
import Data.Active (Active, Duration, Dynamic, Time)
import Data.Map (Map)
import Graphics.Gloss (Color, Picture)
import Graphics.Gloss.Export.Image (Size)
import qualified Codec.Picture as JuicyPixels
import qualified Codec.Picture.Gif as JuicyPixels
import qualified Data.Active as Active
import qualified Data.ByteString as ByteString
import qualified Data.Map as Map
import qualified Graphics.Gloss.Export.Gif as GlossExport
import qualified Graphics.Gloss.Juicy as GlossJuicy

import qualified Data.Active.Extra as Active


readRawGif
  :: FilePath
  -> IO [(Duration Rational, DynamicImage)]
readRawGif filePath = do
  bytes <- ByteString.readFile filePath
  case (,) <$> JuicyPixels.decodeGifImages bytes
           <*> JuicyPixels.getDelaysGifImages bytes of
    Left e -> error e
    Right (frames, centisecondDelays) -> do
      let fromCentiseconds :: Int -> Rational
          fromCentiseconds = (/ 100) . fromIntegral
      let delays :: [Duration Rational]
          delays = fmap
            (Active.toDuration . fromCentiseconds)
            centisecondDelays
      pure $ zip delays frames

dynamicSize :: DynamicImage -> Size
dynamicSize img
  = JuicyPixels.dynamicMap imageSize img
  where
    imageSize :: Pixel pixel => Image pixel -> Size
    imageSize image
      = ( JuicyPixels.imageWidth image
        , JuicyPixels.imageHeight image
        )

readGif
  :: FilePath
  -> IO (Size, Active Picture)
readGif filePath = do
  rawGif <- readRawGif filePath
  let delays :: [Duration Rational]
      delays = fmap fst rawGif
  let frames :: [DynamicImage]
      frames = fmap snd rawGif
  let timestamps :: [Time Rational]
      timestamps
        = fmap Active.toTime
        . scanl (+) 0
        . fmap Active.fromDuration
        $ delays
  let fromDynamicImage :: DynamicImage -> Picture
      fromDynamicImage dynamicImage
        = case GlossJuicy.fromDynamicImage dynamicImage of
            Nothing -> error "readGif: unrecognized image format"
            Just picture -> picture
  let pictures :: [Picture]
      pictures = fmap fromDynamicImage frames
  let table :: Map (Time Rational) Picture
      table = Map.fromList $ zip timestamps pictures
  let getPicture :: Time Rational -> Picture
      getPicture t = case Map.lookupLE t table of
        Nothing -> error "readGif: Active accessed outside of its Era"
        Just (_, frame) -> frame
  let size = dynamicSize (head frames)
  let active = Active.mkActive
        (head timestamps)
        (last timestamps)
        getPicture
  pure (size, active)

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
