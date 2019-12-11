{-# LANGUAGE RecordWildCards #-}
module AnimatedTiling where

import Control.Lens
import Data.Maybe (fromMaybe)
import Graphics.Gloss (Picture(Bitmap))
import Graphics.Gloss.Juicy (loadJuicy)
import Linear.V2 (V2(V2), _x, _y)
import Linear.Vector (negated, (^+^), (^-^))
import qualified Graphics.Gloss as Gloss

import qualified GlossToGif


-- |
-- The image is drawn at the following coordinates:
--
-- > ...
-- > ..., (offsetX - 1 * repeatX, offsetY - 1 * repeatY), (offsetX + 0 * repeatX, offsetY - 1 * repeatY), (offsetX + 1 * repeatX, offsetY + 0 * repeatY), ...
-- > ..., (offsetX - 1 * repeatX, offsetY + 0 * repeatY), (offsetX + 0 * repeatX, offsetY + 0 * repeatY), (offsetX + 1 * repeatX, offsetY + 0 * repeatY), ...
-- > ..., (offsetX - 1 * repeatX, offsetY + 1 * repeatY), (offsetX + 0 * repeatX, offsetY + 1 * repeatY), (offsetX + 1 * repeatX, offsetY + 1 * repeatY), ...
-- > ...
--
-- Where (0, 0) is at the center of the output image, and the anchor point of the image is at its center.
-- +X points right and +Y points up.
data Tile = Tile
  { tileOffset :: (Int, Int)
  , tileRepeat :: (Int, Int)
  , tileImage  :: FilePath
  }

translate
  :: V2 Int
  -> Picture
  -> Picture
translate v
  = Gloss.translate
      (v ^. _x . to fromIntegral)
      (v ^. _y . to fromIntegral)

halved
  :: V2 Int
  -> V2 Int
halved
  = each %~ (`div` 2)

-- |
-- like 'div', but rounds up instead of down
--
-- >>> [-12..12] / 4
cdiv
  :: Int -> Int -> Int
cdiv x y
  = ceiling (fromIntegral x / fromIntegral y)

writeGif
  :: FilePath
  -> Tile
  -> IO ()
writeGif outputFilePath (Tile {..}) = do
  picture@(Bitmap imageWidth imageHeight _ _)
    <- fromMaybe (error $ "loadJuicy: could not open " ++ show tileImage)
   <$> loadJuicy tileImage

  let outputSize  = over both ((7 *) . (`cdiv` 2)) tileRepeat
      outputSizeV = uncurry V2 outputSize
      offsetV     = uncurry V2 tileOffset
      repeatV     = uncurry V2 tileRepeat
      imageSizeV  = V2 imageWidth imageHeight

      -- when drawing picture centered on a point, points outside this won't
      -- affect the output image
      canvasSizeV = outputSizeV ^+^ imageSizeV

      -- -halfX < offsetX + i * repeatX < halfX
      halfV = halved canvasSizeV
      -- -halfX < offsetX + i * repeatX
      -- -halfX - offsetX < i * repeatX
      -- -(halfX + offsetX) < i * repeatX
      -- -(halfX + offsetX) / repeatX < i
      V2 minI minJ
        = cdiv <$> negated (halfV ^+^ offsetV)
               <*> repeatV
      -- offsetX + i * repeatX < halfX
      -- i * repeatX < halfX - offsetX
      -- i < (halfX - offsetX) / repeatX
      V2 maxI maxJ
        = div <$> (halfV ^-^ offsetV)
              <*> repeatV

      repeatedPicture
        = translate offsetV
        $ mconcat
        $ [ translate dV picture
          | j <- [minJ..maxJ]
          , i <- [minI..maxI]
          , let ijV = V2 i j
          , let dV = (*) <$> ijV <*> repeatV
          ]

  GlossToGif.writeImage outputFilePath outputSize repeatedPicture
