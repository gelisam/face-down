{-# LANGUAGE FlexibleContexts #-}
module FaceTrace.Graphics where

import Control.Lens
import Control.Monad.Reader
import Graphics.Gloss.Data.Picture

import FaceTrace.Frame (Frame)
import FaceTrace.Size
import qualified FaceTrace.Frame as Frame
import qualified Graphics.Gloss.Extra as Gloss


antiRectangle :: MonadReader Size m
              => Float -> Float -> m Picture
antiRectangle w h = do
  size <- ask
  pure $ Gloss.antiRectangle (2 * fromIntegral (size ^. displayWidth))
                             (2 * fromIntegral (size ^. displayHeight))
                             w
                             h

clear :: MonadReader Size m
      => m Picture
clear = do
  size <- ask
  pure $ rectangleSolid (fromIntegral (size ^. displayWidth))
                        (fromIntegral (size ^. displayHeight))

textPicture :: MonadReader Size m
            => String -> m Picture
textPicture s = do
  size <- ask
  pure $ text s
       & translate 0 (-113) -- put the origin at the top-left of the text
       & scale 0.15 0.15    -- a more reasonable font size
       & translate (fromIntegral (-(size ^. displayWidth)) / 2 + 5)  -- at top-left
                   (fromIntegral   (size ^. displayHeight) / 2 - 5)  -- of display

framePicture :: MonadReader Size m
             => Frame -> m Picture
framePicture frame = do
  size <- ask
  pure $ Frame.framePicture frame
       & scale (size ^. scaleX)
               (size ^. scaleY)
