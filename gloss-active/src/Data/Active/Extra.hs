{-# LANGUAGE ViewPatterns #-}
module Data.Active.Extra where

import Data.Active (Duration, Dynamic, Era, Time)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Active as Active
import qualified Data.List.NonEmpty as NonEmpty


timeOf
  :: Dynamic a -> Dynamic (Time Rational)
timeOf active
  = Active.mkDynamic (Active.start era) (Active.end era) id
  where
    era :: Era Rational
    era = Active.era active

durationsToTimestamps
  :: Num a
  => [Duration a] -> NonEmpty (Time a)
durationsToTimestamps
  = fmap Active.toTime
  . NonEmpty.scanl (+) 0
  . fmap Active.fromDuration

timestampsToDurations
  :: Num a
  => NonEmpty (Time a) -> [Duration a]
timestampsToDurations (NonEmpty.toList . fmap Active.fromTime -> timestamps)
  = fmap Active.toDuration
  $ zipWith (-) (drop 1 timestamps) timestamps
