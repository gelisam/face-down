{-# LANGUAGE LambdaCase #-}
module GlossToFlv where

import Control.Arrow ((&&&))
import Data.Active (Active)
import Data.Foldable
import Data.Function (fix)
import Data.List.NonEmpty (NonEmpty(..))
import Graphics.Gloss (Color, Picture)
import Graphics.Gloss.Export.Image (Size)
import qualified Codec.Picture as JuicyPixels
import qualified Data.Active as Active
import qualified Data.List.NonEmpty as NonEmpty
import qualified Graphics.Gloss.Export.Image as Export
import qualified Graphics.Gloss.Juicy as GlossJuicy

import Data.Active.FlipBook
import qualified Data.Active.Extra as Active
import qualified Codec.FFmpeg.Extra as FFmpeg


readFlvFlipBook
  :: FilePath
  -> IO (FlipBook FFmpeg.Frame)
readFlvFlipBook filePath = do
  FFmpeg.withMovieReader filePath $ \h -> do
    timestampedFrames <- fix $ \loop -> do
      FFmpeg.hReadTimestampedFrame h >>= \case
        Just timestampedFrame -> do
          (timestampedFrame :) <$> loop
        Nothing -> do
          pure []
    case timestampedFrames of
      [] -> do
        error $ "readFlv " ++ show filePath ++ ": too few frames (0)"
      tf:tfs -> do
        let (timestamps, frames) = NonEmpty.unzip (tf :| tfs)
        let initDurations = Active.timestampsToDurations timestamps
        -- ffmpeg-light only gives the start time of every frame, therefore we
        -- have no way to know how long the last frame lasts. just assume it
        -- lasts the same as the penultimate frame.
        case initDurations ++ [last initDurations] of
          [] -> do
            error $ "readFlv " ++ show filePath ++ ": too few frames (1)"
          d:ds -> do
            let durations = d :| ds
            pure $ NonEmpty.zip durations frames

readFlv
  :: FilePath
  -> IO (Size, Active Picture)
readFlv filePath = do
  flipBook <- readFlvFlipBook filePath
  let frames :: Active FFmpeg.Frame
      frames = runFlipBook flipBook
  let pictures :: Active Picture
      pictures = GlossJuicy.fromImageRGB8 <$> frames
  let size = imageSize . snd . NonEmpty.head $ flipBook
  pure (size, pictures)

imageSize
  :: FFmpeg.Frame
  -> Size
imageSize
    = JuicyPixels.imageWidth
  &&& JuicyPixels.imageHeight

writeFlv
  :: FilePath
  -> Size
  -> Color  -- ^ background color
  -> Int  -- ^ frames per second
  -> Active Picture
  -> IO ()
writeFlv filePath size bg fps active = do
  FFmpeg.withMovieWriter filePath size fps $ \h -> do
    Export.withGlossState size $ \glossState -> do
      for_ frames $ \frame -> do
        Export.withImage size bg glossState frame $ \image -> do
          FFmpeg.hWriteFrame h image
  where
    frames :: [Picture]
    frames
      = Active.simulate (fromIntegral fps) active
