module FaceTrace.VideoInfo where

import Codec.FFmpeg.Probe
import Data.Ratio


-- | (width, height)
videoDimentions :: FilePath -> IO (Int, Int)
videoDimentions filePath = do
  withAvFile filePath $ do
    withStream 0 $ do
      Just avCodecContext <- codecContext
      streamImageSize avCodecContext

-- | width:height
videoPixelAspectRatio :: FilePath -> IO (Maybe (Ratio Int))
videoPixelAspectRatio filePath = do
  withAvFile filePath $ do
    withStream 0 $ do
      Just avCodecContext <- codecContext
      streamSampleAspectRatio avCodecContext
