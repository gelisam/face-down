{-# LANGUAGE ViewPatterns #-}
module Data.Active.Extra where

import Data.Active (Active, Duration, Dynamic, Era, Time)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Active as Active
import qualified Data.List.NonEmpty as NonEmpty


-- Active.simulate samples between the frames, so if you ask for 10 fsp you get
-- 11 samples. This implementation samples the middle of the frames, so you'd
-- get 10 samples.
--
-- Also, if the sample rate is such that the end is not at a sample point,
-- Active.simulate takes one last sample after the end, whereas this
-- implementation does not sample outside the era.
sampleFrames
  :: Rational  -- ^ frames per second
  -> Active a
  -> NonEmpty a
sampleFrames fps
  = Active.onActive
      (\a -> a :| [])
      (sampleFramesDynamic fps)

sampleFramesDynamic
  :: Rational  -- ^ frames per second
  -> Dynamic a
  -> NonEmpty a
sampleFramesDynamic fps dynamic
  = case timestamps of
      [] -> Active.runDynamic dynamic start :| []
      t:ts -> fmap (Active.runDynamic dynamic) (t :| ts)
  where
    spf :: Rational
    spf = 1 / fps

    start :: Time Rational
    start = Active.start . Active.era $ dynamic

    end :: Time Rational
    end = Active.end . Active.era $ dynamic

    firstTimestamp :: Time Rational
    firstTimestamp = Active.toTime (spf / 2)

    timestamps :: [Time Rational]
    timestamps
      = filter (<= end)
          [ firstTimestamp
          , firstTimestamp + Active.toTime spf
         .. end
          ]

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
