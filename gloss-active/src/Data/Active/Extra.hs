{-# LANGUAGE ViewPatterns #-}
module Data.Active.Extra where

import Data.Active (Active, Duration, Dynamic, Era, Time)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Active as Active
import qualified Data.List.NonEmpty as NonEmpty


-- Active.simulate always returns a non-empty list, its type just doesn't
-- reflect it.
simulateNonEmpty
  :: Rational  -- ^ frames per second
  -> Active a
  -> NonEmpty a
simulateNonEmpty fps active
  = case Active.simulate fps active of
      [] -> error "never happens: simulate always returns a non-empty list"
      a:as -> a :| as

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
