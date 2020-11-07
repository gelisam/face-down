{-# LANGUAGE ScopedTypeVariables #-}
module Data.Active.FlipBook where

import Data.Active (Active, Duration, Time)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import qualified Data.Active as Active
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map

import qualified Data.Active.Extra as Active


type Page a
  = (Duration Rational, a)
type FlipBook a
  = NonEmpty (Page a)

runFlipBook
  :: forall a. FlipBook a -> Active a
runFlipBook flipBook = Active.mkActive begin end getPage
  where
    durations :: [Duration Rational]
    durations
      = NonEmpty.toList
      . fmap fst
      $ flipBook

    pages :: NonEmpty a
    pages = fmap snd flipBook

    timestamps :: NonEmpty (Time Rational)
    timestamps
      = Active.durationsToTimestamps durations

    begin :: Time Rational
    begin = NonEmpty.head timestamps

    end :: Time Rational
    end = NonEmpty.last timestamps

    table :: Map (Time Rational) a
    table = Map.fromList
          . NonEmpty.toList
          $ NonEmpty.zip timestamps pages

    getPage :: Time Rational -> a
    getPage t = case Map.lookupLE t table of
      Nothing -> error $ "runFlipBook: Active accessed outside of its Era "
                      ++ "(" ++ show t
                      ++ " is not within " ++ show [begin, end]
                      ++ ")"
      Just (_, page) -> page
