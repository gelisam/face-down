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
  -> Tile
  -> TestTree
mkGolden basename tile
  = goldenVsFile
      basename
      expected
      actual
      (writeGif actual tile)
  where
    expected = test </> "expected" </> basename <.> "gif"
    actual   = test </> "actual"   </> basename <.> "gif"

test :: FilePath
test = $(TH.lift =<< pathRelativeToCabalPackage "test")

inputs :: FilePath
inputs = test </> "inputs"

tests
  :: TestTree
tests
  = testGroup "animated-tiling Golden Tests"
  [ mkGolden "9-oranges"
    $ Tile (0, 0) (48, 48) (inputs </> "orange.png")
  , mkGolden "16-oranges"
    $ Tile (24, 24) (48, 48) (inputs </> "orange.png")
  , mkGolden "packed-oranges"
    $ Tile (0, 0) (32, 32) (inputs </> "orange.png")
  , mkGolden "honey"
    $ Tile (0, 0) (16, 16) (inputs </> "orange.png")
  ]
