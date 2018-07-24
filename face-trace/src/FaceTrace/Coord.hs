{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
module FaceTrace.Coord where

import Control.Lens
import Graphics.Gloss
import Linear


-- gloss coordinates:
-- (0, 0) in the center,
-- (1, 0) one pixel to the right of the center,
-- (0, 1) one pixel above the center
newtype Coord = Coord { unCoord :: V2 Float }
  deriving (Eq, Num, Show)

makePrisms ''Coord

coordX :: Lens' Coord Float
coordX = _Coord . _x

coordY :: Lens' Coord Float
coordY = _Coord . _y


toPoint :: Coord -> Point
toPoint (Coord (V2 x y)) = (x, y)
