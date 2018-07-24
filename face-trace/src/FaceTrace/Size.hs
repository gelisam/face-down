{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}
module FaceTrace.Size where

import Control.Lens
import Control.Monad.Reader
import Linear

import FaceTrace.Coord
import FaceTrace.Rel


-- in pixels
newtype Size = Size { unSize :: V2 Float }
  deriving (Eq, Show)

makePrisms ''Size

sizeWidth :: Lens' Size Float
sizeWidth = _Size . _x

sizeHeight :: Lens' Size Float
sizeHeight = _Size . _y


toCoord :: MonadReader Size m
        => Rel -> m Coord
toCoord (Rel v) = do
  size <- ask
  pure $ v
       & subtract (V2 0.5 0.5)  -- (0, 0) in the center
       & (* V2 1 (-1))          -- +Y pointing up
       & (* (unSize size / 2))
       & Coord

toRel :: MonadReader Size m
      => Coord -> m Rel
toRel (Coord v) = do
  size <- ask
  pure $ v
       & (/ (unSize size / 2))
       & (* V2 1 (-1))     -- +Y pointing down
       & (+ (V2 0.5 0.5))  -- (0, 0) in the top left
       & Rel
