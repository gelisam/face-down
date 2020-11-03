module GlossActive where

import Data.Active
import Graphics.Gloss (Color, Display, Picture)
import qualified Graphics.Gloss as Gloss


animateActive
  :: Display
  -> Color  -- ^ background color
  -> Active Picture
  -> IO ()
animateActive display bg active = do
  Gloss.animate
    display
    bg
    (runActive active . toTime . realToFrac)
