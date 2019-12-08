{-# LANGUAGE TemplateHaskell #-}
module Main where

import System.FilePath
import Test.Tasty
import Test.Tasty.Golden
import TH.RelativePaths
import qualified Language.Haskell.TH.Syntax as TH

import AnimatedTiling


main :: IO ()
main = defaultMain tests

mkGolden
  :: FilePath
  -> TestTree
mkGolden basename
  = goldenVsFile
      basename
      expected
      actual
      (writeGif actual)
  where
    test = $(TH.lift =<< pathRelativeToCabalPackage "test")
    expected = test </> "expected" </> basename <.> "gif"
    actual   = test </> "actual"   </> basename <.> "gif"

tests
  :: TestTree
tests
  = testGroup "animated-tiling Golden Tests" []
