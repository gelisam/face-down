{-# LANGUAGE LambdaCase #-}
module Codec.FFmpeg.Extra where

import Codec.Picture
import Control.Exception
import Data.Active (Time)
import Graphics.Gloss.Export.Image (Size)
import qualified Codec.FFmpeg as FFmpeg
import qualified Data.Active as Active


type Frame = Image PixelRGB8
type MovieReaderHandle = (IO (Maybe (Frame, Double)), IO ())
type MovieWriterHandle = Maybe Frame -> IO ()

hReadTimestampedFrame :: MovieReaderHandle -> IO (Maybe (Time Rational, Frame))
hReadTimestampedFrame (readPage, _) = do
  readPage >>= \case
    Nothing -> do
      pure Nothing
    Just (frame, timestampDouble) -> do
      let timestamp = Active.toTime . realToFrac $ timestampDouble
      pure $ Just (timestamp, frame)

hReadFrame :: MovieReaderHandle -> IO (Maybe Frame)
hReadFrame h = fmap snd <$> hReadTimestampedFrame h

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
