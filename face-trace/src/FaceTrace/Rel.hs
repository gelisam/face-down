{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
module FaceTrace.Rel where

import Control.Lens
import Linear


-- scaling-invariant video coordinates:
-- (0, 0) at the top left,
-- (1, 1) at the bottom right
newtype Rel = Rel { unRel :: V2 Float }
  deriving (Eq, Num, Show)

makePrisms ''Rel

relX :: Lens' Rel Float
relX = _Rel . _x

relY :: Lens' Rel Float
relY = _Rel . _y
