{-# LANGUAGE LambdaCase, TemplateHaskell #-}
module FaceTrace.Size where

import Control.Lens
import Data.Maybe
import Data.Ratio
import Graphics.Gloss.Data.Display

import FaceTrace.VideoInfo


data Size = Size
  { _pixelWidth    :: Int
  , _pixelHeight   :: Int
  , _displayWidth  :: Int
  , _displayHeight :: Int
  , _scaleX        :: Float  -- displayWidth  / pixelWidth
  , _scaleY        :: Float  -- displayHeight / pixelHeight
  }

makeLenses ''Size


videoSize :: FilePath -> IO Size
videoSize filePath = do
  (pixelWidth_, pixelHeight_) <- videoDimentions filePath
  pixelAspectRatio <- videoPixelAspectRatio filePath
                  <&> fromMaybe 1
  let displayAspectRatio :: Ratio Int
      displayAspectRatio = (pixelWidth_  * numerator pixelAspectRatio)
                         % (pixelHeight_ * denominator pixelAspectRatio)

      displayWidth_ :: Int
      displayWidth_ = div pixelWidth_ <$> [1..]
                    & dropWhile (> 1024)
                    & head

      displayHeight_ :: Int
      displayHeight_ = round (fromIntegral displayWidth_ / displayAspectRatio)

      scaleX_ :: Float
      scaleX_ = fromIntegral displayWidth_ / fromIntegral pixelWidth_

      scaleY_ :: Float
      scaleY_ = fromIntegral displayHeight_ / fromIntegral pixelHeight_

  pure $ Size pixelWidth_
              pixelHeight_
              displayWidth_
              displayHeight_
              scaleX_
              scaleY_

sizedDisplay :: String -> Size -> Display
sizedDisplay windowTitle size
  = InWindow windowTitle
             (size ^. displayWidth, size ^. displayHeight)
             (10, 10)