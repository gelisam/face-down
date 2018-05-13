module FaceTrace.Interpolate where

import Control.Lens
import Data.Map.Strict (Map)
import Data.Spline
import Data.Traversable
import Linear.V3
import qualified Data.Map.Strict as Map

import FaceTrace.FaceState
import FaceTrace.Types


type InterpolationPoint = V3 Double

_t :: Lens' InterpolationPoint Double
_t = _z

toInterpolationPoint :: Double -> Pos -> InterpolationPoint
toInterpolationPoint t (x,y) = V3 (realToFrac x) (realToFrac y) t

fromInterpolationPoint :: InterpolationPoint -> Pos
fromInterpolationPoint p = ( round (p ^. _x)
                           , round (p ^. _y)
                           )

interpolate :: Map FaceTimestamp Pos -> Double -> Maybe Pos
interpolate facePositions timestamp = do
  tMin <- fst <$> Map.lookupMin facePositions
  tMax <- fst <$> Map.lookupMax facePositions

  -- a few keyframes around the sampled point are sufficient
  let t0 = timestamp
         & previousFaceTimestamp
         & previousFaceTimestamp
         & max tMin
  let ts = take 6 $ iterate (min tMax . nextFaceTimestamp) t0

  keys <- for ts $ \t -> do
    coord <- facePositions ^. at t
    pure $ Cosine $ toInterpolationPoint t coord
  let spline_ = spline (^. _t) keys
  point <- sample (^. _t) spline_ timestamp
  pure $ fromInterpolationPoint point

followFace :: Map FaceTimestamp Pos -> Double -> Maybe Pos
followFace facePositions t = do
  -- add a bit of lag to make it look like the cameraman is reacting to the
  -- speaker's movements instead of anticipating it.
  interpolate facePositions (max 0 (t - 0.5))
