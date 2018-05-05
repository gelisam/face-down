module Graphics.Gloss.Extra where

import Data.Function
import Data.Monoid
import Graphics.Gloss


rectangleLowerSolid :: Float -> Float -> Picture
rectangleLowerSolid w h = rectangleUpperSolid w h
                        & rotate 180

rectangleLeftSolid :: Float -> Float -> Picture
rectangleLeftSolid w h = rectangleUpperSolid h w
                       & rotate (-90)

rectangleRightSolid :: Float -> Float -> Picture
rectangleRightSolid w h = rectangleUpperSolid h w
                        & rotate 90

antiRectangle :: Float -> Float -> Float -> Float -> Picture
antiRectangle ww hh w h = (rectangleUpperSolid ww (hh / 2 - h / 2) & translate 0 ( h / 2))
                       <> (rectangleLowerSolid ww (hh / 2 - h / 2) & translate 0 (-h / 2))
                       <> (rectangleLeftSolid  (ww / 2 - w / 2) h  & translate (-w / 2) 0)
                       <> (rectangleRightSolid (ww / 2 - w / 2) h  & translate ( w / 2) 0)
