module FaceTrace.Interpolate where

import Control.Lens
import Data.Map.Strict (Map)
import Data.Spline
import Data.Traversable
import Linear
import qualified Data.Map.Strict as Map

import FaceTrace.FaceState
import FaceTrace.Rel


type InterpolationPoint = V3 Double

_t :: Lens' InterpolationPoint Double
_t = _z

toInterpolationPoint :: Double -> Rel -> InterpolationPoint
toInterpolationPoint t (Rel (V2 x y)) = V3 (realToFrac x) (realToFrac y) t

fromInterpolationPoint :: InterpolationPoint -> Rel
fromInterpolationPoint p = Rel (V2 (realToFrac (p ^. _x))
                                   (realToFrac (p ^. _y)))

interpolate :: Map FaceTimestamp Rel -> Double -> Maybe Rel
interpolate relFacePositions timestamp = do
  tMin <- fst <$> Map.lookupMin relFacePositions
  tMax <- fst <$> Map.lookupMax relFacePositions

  -- a few keyframes around the sampled point are sufficient
  let t0 = timestamp
         & previousFaceTimestamp
         & previousFaceTimestamp
         & max tMin
  let ts = take 6 $ iterate (min tMax . nextFaceTimestamp) t0

  keys <- for ts $ \t -> do
    coord <- relFacePositions ^. at t
    pure $ Cosine $ toInterpolationPoint t coord
  let spline_ = spline (^. _t) keys
  point_ <- sample (^. _t) spline_ timestamp
  pure $ fromInterpolationPoint point_

followFace :: Map FaceTimestamp Rel -> Double -> Maybe Rel
followFace relFacePositions t = do
  -- add a bit of lag to make it look like the cameraman is reacting to the
  -- speaker's movements instead of anticipating it.
  interpolate relFacePositions (max 0 (t - 0.5))
