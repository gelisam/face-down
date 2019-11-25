{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Active
import Data.Functor.Apply
import Graphics.Gloss
import Graphics.Gloss.Export.Gif

import GlossToGif


main
  :: IO ()
main = do
  renderGif "out.gif" (300, 300) LoopingForever white dynamic


sine
  :: Floating a
  => Dynamic a
sine
  = mkDynamic 0 1 $ \t -> sin (2 * pi * realToFrac t)

cycleBetween
  :: forall a. Floating a
  => a  -- ^ a1
  -> a  -- ^ seconds between a1 and a2
  -> a  -- ^ a2
  -> Dynamic a
cycleBetween a1 dt a2
  = (\t -> a1 + (a2-a1) * (1 + t / period) / 2) <$> sine
  where
    period :: a
    period = 2 * dt

dynamic :: Dynamic Picture
dynamic
    = thickCircle
  <$> cycleBetween 50 0.5 140
  <.> cycleBetween 1 0.5 10
