{-# LANGUAGE LambdaCase #-}
module FaceTrace.VideoInfo where

import Codec.FFmpeg
import Codec.FFmpeg.Probe
import Control.Monad.IO.Class
import Data.Ratio (Ratio, (%))

import Control.Monad.Extra


-- | (width, height)
videoDimentions :: FilePath -> IO (Int, Int)
videoDimentions filePath = do
  withAvFile filePath $ do
    withStream 0 $ do
      ~(Just avCodecContext) <- codecContext
      streamImageSize avCodecContext

-- | width:height
videoPixelAspectRatio :: FilePath -> IO (Ratio Int)
videoPixelAspectRatio filePath = do
  withAvFile filePath $ do
    withStream 0 $ do
      ~(Just avCodecContext) <- codecContext

      -- default to 1, meaning "square pixels"
      liftIO $ getAspectRatio avCodecContext <&> \case
        Nothing -> 1
        Just aspectRatio -> ( fromIntegral (numerator   aspectRatio)
                            % fromIntegral (denomenator aspectRatio)
                            )
