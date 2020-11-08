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

mkTests
  :: FilePath
  -> (Int, Int)
  -> Color
  -> Int
  -> Active Picture
  -> TestTree
mkTests basename size bg fps animation = do
  testGroup basename
    [ testCase "matches golden file" $ do
        writeGif actualFilePath size bg fps animation
        expectedGif <- readGifFlipBook expectedFilePath
        actualGif <- readGifFlipBook actualFilePath
        when (expectedGif /= actualGif) $ do
          assertFailure $ "files differ."
    , testCase "round-trips" $ do
        writeGif actualFilePath size bg fps animation
        (size2, animation2) <- readGif actualFilePath
        writeGif actualFilePath size2 bg fps animation2
        expectedGif <- readGifFlipBook expectedFilePath
        actualGif <- readGifFlipBook actualFilePath
        when (expectedGif /= actualGif) $ do
          assertFailure $ "files differ."
    ]
  where
    test = $(TH.lift =<< pathRelativeToCabalPackage "test")
    expectedFilePath = test </> "expected" </> basename <.> "gif"
    actualFilePath   = test </> "actual"   </> basename <.> "gif"

tests
  :: TestTree
tests
  = testGroup "gloss-to-gif Golden Tests"
  [ mkTests "pulsating-circle" (300, 300) white 60 pulsatingCircle
  ]
