{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module FaceTrace.FaceState where

import Control.Lens
import Data.Acid
import Data.Map.Strict (Map)
import Data.SafeCopy
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Control.Monad.Extra


-- offset from top-left
type Pos = (Int, Int)

-- seconds since the beginning of the movie
type Timestamp = Double

data FaceState = FaceState
  { _faceStateFacePositions    :: Map Timestamp Pos
  , _faceStateSlideTransitions :: Set Timestamp
  } 

deriveSafeCopy 0 'base ''FaceState
makeLenses ''FaceState


-- we want a face position every 2 seconds
nearestFaceTimestamp :: Double -> Double
nearestFaceTimestamp = (/ 2)
                   >>> (round :: Double -> Int)
                   >>> (* 2)
                   >>> fromIntegral

nextFaceTimestamp :: Double -> Double
nextFaceTimestamp = (+ 2)
                >>> nearestFaceTimestamp

previousFaceTimestamp :: Double -> Double
previousFaceTimestamp = subtract 2
                    >>> max 0
                    >>> nearestFaceTimestamp


getFacePositions :: Query FaceState (Map Timestamp Pos)
getFacePositions = view faceStateFacePositions

insertFacePosition :: Timestamp -> Pos -> Update FaceState ()
insertFacePosition t pos = faceStateFacePositions %= Map.insert t pos

deleteFacePosition :: Timestamp -> Update FaceState ()
deleteFacePosition t = faceStateFacePositions %= Map.delete t


getSlideTransitions :: Query FaceState (Set Timestamp)
getSlideTransitions = view faceStateSlideTransitions

insertSlideTransition :: Timestamp -> Update FaceState ()
insertSlideTransition t = faceStateSlideTransitions %= Set.insert t

deleteSlideTransition :: Timestamp -> Update FaceState ()
deleteSlideTransition t = faceStateSlideTransitions %= Set.delete t


makeAcidic ''FaceState
  [ 'getFacePositions   , 'insertFacePosition   , 'deleteFacePosition
  , 'getSlideTransitions, 'insertSlideTransition, 'deleteSlideTransition
  ]
