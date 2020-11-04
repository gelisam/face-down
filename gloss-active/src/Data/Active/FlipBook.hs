{-# LANGUAGE ScopedTypeVariables #-}
module Data.Active.FlipBook where

import Data.Active (Active, Duration, Time)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import qualified Data.Active as Active
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map


type FlipBook a
  = NonEmpty ((Duration Rational, a))

runFlipBook
  :: forall a. FlipBook a -> Active a
runFlipBook flipBook = Active.mkActive
  (NonEmpty.head timestamps)
  (NonEmpty.last timestamps)
  getPage
  where
    durations :: NonEmpty (Duration Rational)
    durations = fmap fst flipBook

    pages :: NonEmpty a
    pages = fmap snd flipBook

    timestamps :: NonEmpty (Time Rational)
    timestamps
      = fmap Active.toTime
      . NonEmpty.scanl (+) 0
      . fmap Active.fromDuration
      $ durations

    table :: Map (Time Rational) a
    table = Map.fromList
          . NonEmpty.toList
          $ NonEmpty.zip timestamps pages

    getPage :: Time Rational -> a
    getPage t = case Map.lookupLE t table of
      Nothing -> error "runFlipBook: Active accessed outside of its Era"
      Just (_, page) -> page
