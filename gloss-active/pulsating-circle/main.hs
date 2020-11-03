module Main where

import Graphics.Gloss

import GlossActive
import GlossActive.TestSupport


main :: IO ()
main = do
  animateActive
    (InWindow "title" (300,300) (300,300))
    white
    pulsatingCircle
