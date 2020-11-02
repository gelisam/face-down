{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module Main where

import Control.Monad
import Data.Active
import Graphics.Gloss
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit
import TH.RelativePaths
import qualified Language.Haskell.TH.Syntax as TH

import GlossToGif


main :: IO ()
main = defaultMain tests

mkGolden
  :: FilePath
  -> (Int, Int)
  -> Color
  -> Int
  -> Active Picture
  -> TestTree
mkGolden basename size bg fps animation = testCase basename $ do
  writeGif actualFilePath size bg fps animation
  expectedGif <- readGif expectedFilePath
  actualGif <- readGif actualFilePath
  when (expectedGif /= actualGif) $ do
    assertFailure $ "files differ."
  where
    test = $(TH.lift =<< pathRelativeToCabalPackage "test")
    expectedFilePath = test </> "expected" </> basename <.> "gif"
    actualFilePath   = test </> "actual"   </> basename <.> "gif"

tests
  :: TestTree
tests
  = testGroup "gloss-to-gif Golden Tests"
  [ mkGolden "pulsating-circle" (300, 300) white 25
      $ translate
    <$> cycleBetween 50 0.5 0
    <*> cycleBetween 95 0.5 0
    <*> ( thickCircle
      <$> cycleBetween 50 0.5 140
      <*> cycleBetween 1 0.5 10
        )
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
