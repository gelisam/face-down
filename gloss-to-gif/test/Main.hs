{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module Main where

import Data.Active
import Graphics.Gloss
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden
import TH.RelativePaths
import qualified Language.Haskell.TH.Syntax as TH

import GlossToGif


main :: IO ()
main = defaultMain tests

mkGolden
  :: FilePath
  -> (Int, Int)
  -> Active Picture
  -> TestTree
mkGolden basename size animation
  = goldenVsFile
      basename
      expected
      actual
      (writeGif actual size animation)
  where
    test = $(TH.lift =<< pathRelativeToCabalPackage "test")
    expected = test </> "expected" </> basename <.> "gif"
    actual   = test </> "actual"   </> basename <.> "gif"

tests
  :: TestTree
tests
  = testGroup "gloss-to-gif Golden Tests"
  [ mkGolden "pulsating-circle" (300, 300)
      $ thickCircle
    <$> cycleBetween 50 0.5 140
    <*> cycleBetween 1 0.5 10
  ]


sine
  :: Floating a
  => Active a
sine
  = mkActive 0 1 $ \t -> sin (2 * pi * realToFrac t)

cycleBetween
  :: forall a. Floating a
  => a  -- ^ a1
  -> a  -- ^ seconds between a1 and a2
  -> a  -- ^ a2
  -> Active a
cycleBetween a1 dt a2
  = (\t -> a1 + (a2-a1) * (1 + t / period) / 2) <$> sine
  where
    period :: a
    period = 2 * dt
