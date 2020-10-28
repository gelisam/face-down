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
import GlossToFlv


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
  writeFlv actualFilePath size bg fps animation
  expectedFlv <- readFlvFlipBook expectedFilePath
  actualFlv <- readFlvFlipBook actualFilePath
  when (expectedFlv /= actualFlv) $ do
    assertFailure $ "files differ."
  where
    test = $(TH.lift =<< pathRelativeToCabalPackage "test")
    expectedFilePath = test </> "expected" </> basename <.> "flv"
    actualFilePath   = test </> "actual"   </> basename <.> "flv"

tests
  :: TestTree
tests
  = testGroup "gloss-to-flv Golden Tests"
  [ mkGolden "pulsating-circle" (300, 300) white 60 pulsatingCircle
  ]
