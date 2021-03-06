{-# LANGUAGE ScopedTypeVariables #-}
module GlossActive.TestSupport where

import Data.Active
import Graphics.Gloss


pulsatingCircle :: Active Picture
pulsatingCircle
    = translate
  <$> cycleBetween 50 0.5 0
  <*> cycleBetween 95 0.5 0
  <*> ( thickCircle
    <$> cycleBetween 50 0.5 140
    <*> cycleBetween 1 0.5 10
      )


sine
  :: Floating a
  => Active a
sine
  = mkActive 0 1 $ \t -> sin (2 * pi * realToFrac t)

cycleBetween
  :: forall a. Floating a
  => a  -- ^ a1
  -> a  -- ^ seconds between a1 and a2
  -> a  -- ^ a2
  -> Active a
cycleBetween a1 dt a2
  = (\t -> a1 + (a2-a1) * (1 + t / period) / 2) <$> sine
  where
    period :: a
    period = 2 * dt
