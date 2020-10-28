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
import qualified Codec.FFmpeg.Extra as FFmpeg


readFlvFlipBook
  :: FilePath
  -> IO (FlipBook FFmpeg.Frame)
readFlvFlipBook filePath = do
  FFmpeg.withMovieReader filePath $ \h -> do
    pages <- fix $ \loop -> do
      FFmpeg.hReadPage h >>= \case
        Just page -> do
          (page :) <$> loop
        Nothing -> do
          pure []
    case pages of
      [] -> do
        error $ "readFlv " ++ show filePath ++ ": 0 frames"
      p:ps -> do
        pure (p :| ps)

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
