{-# LANGUAGE ViewPatterns #-}
module GlossToGif where

import Codec.Picture (DynamicImage, Image, Pixel)
import Data.Active (Active, Duration, Dynamic)
import Data.List.NonEmpty (NonEmpty)
import Graphics.Gloss (Color, Picture)
import Graphics.Gloss.Export.Image (Size)
import qualified Codec.Picture as JuicyPixels
import qualified Codec.Picture.Gif as JuicyPixels
import qualified Data.Active as Active
import qualified Data.ByteString as ByteString
import qualified Data.List.NonEmpty as NonEmpty
import qualified Graphics.Gloss.Export.Gif as GlossExport
import qualified Graphics.Gloss.Juicy as GlossJuicy

import Data.Active.FlipBook
import qualified Data.Active.Extra as Active


readGifFlipBook
  :: FilePath
  -> IO (FlipBook DynamicImage)
readGifFlipBook filePath = do
  bytes <- ByteString.readFile filePath
  case (,) <$> JuicyPixels.decodeGifImages bytes
           <*> JuicyPixels.getDelaysGifImages bytes of
    Left e -> do
      error e
    Right ( NonEmpty.nonEmpty -> Just frames
          , NonEmpty.nonEmpty -> Just centisecondDelays) -> do
      let fromCentiseconds :: Int -> Rational
          fromCentiseconds = (/ 100) . fromIntegral
      let delays :: NonEmpty (Duration Rational)
          delays = fmap
            (Active.toDuration . fromCentiseconds)
            centisecondDelays
      pure $ NonEmpty.zip delays frames
    _ -> do
      error $ "readGif " ++ show filePath ++ ": 0 frames"

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
  flipBook <- readGifFlipBook filePath
  let dynamicImages :: Active DynamicImage
      dynamicImages = runFlipBook flipBook
  let fromDynamicImage :: DynamicImage -> Picture
      fromDynamicImage dynamicImage
        = case GlossJuicy.fromDynamicImage dynamicImage of
            Nothing -> error "readGif: unrecognized image format"
            Just picture -> picture
  let pictures :: Active Picture
      pictures = fmap fromDynamicImage dynamicImages
  let size = dynamicSize . snd . NonEmpty.head $ flipBook
  pure (size, pictures)

writeGif
  :: FilePath
  -> Size
  -> Color  -- ^ background color
  -> Int  -- ^ frames per second
  -> Active Picture
  -> IO ()
writeGif filePath size bg fps = do
  Active.onActive (writeGifImage filePath size bg)
                  (writeGifLoop  filePath size bg fps)

writeGifImage
  :: FilePath
  -> Size
  -> Color  -- ^ background color
  -> Picture
  -> IO ()
writeGifImage filePath size bg picture = do
  GlossExport.exportPicturesToGif
    0
    GlossExport.LoopingNever
    size
    bg
    filePath
    (const picture)
    [0]

writeGifLoop
  :: FilePath
  -> Size
  -> Color  -- ^ background color
  -> Int  -- ^ frames per second
  -> Dynamic Picture
  -> IO ()
writeGifLoop filePath size bg fps dynamic = do
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
