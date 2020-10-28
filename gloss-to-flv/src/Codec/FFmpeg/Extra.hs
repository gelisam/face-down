{-# LANGUAGE LambdaCase #-}
module Codec.FFmpeg.Extra where

import Codec.Picture
import Control.Exception
import Graphics.Gloss.Export.Image (Size)
import qualified Codec.FFmpeg as FFmpeg

import Data.Active.FlipBook (Page)


type Frame = Image PixelRGB8
type MovieReaderHandle = (IO (Maybe (Frame, Double)), IO ())
type MovieWriterHandle = Maybe Frame -> IO ()

hReadPage :: MovieReaderHandle -> IO (Maybe (Page Frame))
hReadPage (readPage, _) = do
  readPage >>= \case
    Nothing -> do
      pure Nothing
    Just (frame, doubleDuration) -> do
      pure $ Just (realToFrac doubleDuration, frame)

hReadFrame :: MovieReaderHandle -> IO (Maybe Frame)
hReadFrame h = fmap snd <$> hReadPage h

hWriteFrame :: MovieWriterHandle -> Frame -> IO ()
hWriteFrame f img = f (Just img)

hCloseReader :: MovieReaderHandle -> IO ()
hCloseReader = snd

hCloseWriter :: MovieWriterHandle -> IO ()
hCloseWriter h = h Nothing

withMovieReader
  :: FilePath
  -> (MovieReaderHandle -> IO a)
  -> IO a
withMovieReader filePath body = do
  FFmpeg.initFFmpeg
  bracket
    (FFmpeg.imageReaderTime (FFmpeg.File filePath))
    hCloseReader
    body

withMovieWriter
  :: FilePath
  -> Size
  -> Int  -- ^ frames per second
  -> (MovieWriterHandle -> IO a)
  -> IO a
withMovieWriter filePath (w,h) fps body = do
  FFmpeg.initFFmpeg
  bracket
    (FFmpeg.imageWriter params filePath)
    hCloseWriter
    body
  where
    params :: FFmpeg.EncodingParams
    params
      = (FFmpeg.defaultParams (fromIntegral w) (fromIntegral h))
        { FFmpeg.epFps = fps }
