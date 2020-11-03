module GlossActive where

import Data.Active
import Data.Fixed (mod')
import Graphics.Gloss (Color, Display, Picture)
import qualified Graphics.Gloss as Gloss


animateActive
  :: Display
  -> Color  -- ^ background color
  -> Active Picture
  -> IO ()
animateActive display bg = onActive
  (Gloss.display display bg)
  (animateDynamic display bg)

animateDynamic
  :: Display
  -> Color  -- ^ background color
  -> Dynamic Picture
  -> IO ()
animateDynamic display bg dynamic = do
  Gloss.animate
    display
    bg
    (runDynamic dynamic . toTime . (`mod'` d) . realToFrac)
  where
    d = fromDuration . duration . era $ dynamic
