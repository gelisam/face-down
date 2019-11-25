{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Active
import Graphics.Gloss

import GlossToGif


main
  :: IO ()
main = do
  writeGif "out.gif" (300, 300) active


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

active :: Active Picture
active
    = thickCircle
  <$> cycleBetween 50 0.5 140
  <*> cycleBetween 1 0.5 10
