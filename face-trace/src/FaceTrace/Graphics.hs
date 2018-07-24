{-# LANGUAGE FlexibleContexts #-}
module FaceTrace.Graphics where

import Control.Lens
import Control.Monad.Reader
import Graphics.Gloss.Data.Picture

import FaceTrace.Size
import qualified Graphics.Gloss.Extra as Gloss


antiRectangle :: MonadReader Size m
              => Float -> Float -> m Picture
antiRectangle w h = do
  size <- ask
  pure $ Gloss.antiRectangle (2 * size ^. sizeWidth)
                             (2 * size ^. sizeHeight)
                             w
                             h

clear :: MonadReader Size m
      => m Picture
clear = do
  size <- ask
  pure $ rectangleSolid (size ^. sizeWidth)
                        (size ^. sizeHeight)

textPicture :: MonadReader Size m
            => String -> m Picture
textPicture s = do
  size <- ask
  pure $ text s
       & translate 0 (-113) -- put the origin at the top-left of the text
       & scale 0.15 0.15    -- a more reasonable font size
       & translate (-(size ^. sizeWidth) / 2 + 5)  -- at top-left
                   (  size ^. sizeHeight / 2 - 5)  -- of display
