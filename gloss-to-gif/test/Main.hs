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

import GlossActive.TestSupport
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
  expectedGif <- readRawGif expectedFilePath
  actualGif <- readRawGif actualFilePath
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
  [ mkGolden "pulsating-circle" (300, 300) white 60 pulsatingCircle
  ]
