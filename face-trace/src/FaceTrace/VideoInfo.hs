module FaceTrace.VideoInfo where

import Codec.FFmpeg
import Codec.FFmpeg.Probe
import Control.Monad.IO.Class
import Data.Ratio (Ratio, (%))


-- | (width, height)
videoDimentions :: FilePath -> IO (Int, Int)
videoDimentions filePath = do
  withAvFile filePath $ do
    withStream 0 $ do
      Just avCodecContext <- codecContext
      streamImageSize avCodecContext

-- | width:height
videoPixelAspectRatio :: FilePath -> IO (Ratio Int)
videoPixelAspectRatio filePath = do
  withAvFile filePath $ do
    withStream 0 $ do
      Just avCodecContext <- codecContext

      rawAspectRatio <- liftIO $ getAspectRatio avCodecContext

      -- 0 means "unknown", so we default to 1, meaning "square pixels"
      if numerator rawAspectRatio == 0
      then pure 1
      else pure ( fromIntegral (numerator   rawAspectRatio)
                % fromIntegral (denomenator rawAspectRatio)
                )
