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
import FaceTrace.Types


type FaceTimestamp = Timestamp

data FaceState = FaceState
  { _faceStateFacePositions    :: Map FaceTimestamp Pos
  , _faceStateSlideTransitions :: Set Timestamp
  } 

deriveSafeCopy 0 'base ''FaceState
makeLenses ''FaceState

instance Monoid FaceState where
  mempty = FaceState mempty mempty
  FaceState x1 y1 `mappend` FaceState x2 y2
    = FaceState (x1 `mappend` x2)
                (y1 `mappend` y2)


-- we want a face position every 2 seconds
nearestFaceTimestamp :: Timestamp -> FaceTimestamp
nearestFaceTimestamp = (/ 2)
                   >>> (round :: Timestamp -> Int)
                   >>> (* 2)
                   >>> fromIntegral

nextFaceTimestamp :: Timestamp -> FaceTimestamp
nextFaceTimestamp = (+ 2)
                >>> nearestFaceTimestamp

previousFaceTimestamp :: Timestamp -> FaceTimestamp
previousFaceTimestamp = subtract 2
                    >>> max 0
                    >>> nearestFaceTimestamp


getFacePositions :: Query FaceState (Map FaceTimestamp Pos)
getFacePositions = view faceStateFacePositions

insertFacePosition :: FaceTimestamp -> Pos -> Update FaceState ()
insertFacePosition t pos = faceStateFacePositions %= Map.insert t pos

deleteFacePosition :: FaceTimestamp -> Update FaceState ()
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
