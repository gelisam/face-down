module Data.Active.Extra where

import Data.Active (Dynamic, Era, Time)
import qualified Data.Active as Active


timestamps
  :: Dynamic a -> Dynamic (Time Rational)
timestamps active
  = Active.mkDynamic (Active.start era) (Active.end era) id
  where
    era :: Era Rational
    era = Active.era active
