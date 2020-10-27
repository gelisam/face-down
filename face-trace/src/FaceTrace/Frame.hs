module FaceTrace.Frame where

import Codec.Picture
import Graphics.Gloss.Interface.IO.Animate
import Graphics.Gloss.Juicy


type Frame = Image PixelRGB8

frameWidth :: Frame -> Int
frameWidth = imageWidth

frameHeight :: Frame -> Int
frameHeight = imageHeight

-- don't cache the pictures, or the cache will quickly consume all the RAM
framePicture :: Frame -> Picture
framePicture = fromImageRGB8


defaultFrame :: Frame
defaultFrame = generateImage (\_ _ -> PixelRGB8 0 0 0) 640 480
