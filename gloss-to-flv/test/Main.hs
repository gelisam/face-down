{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module Main where

import Control.Lens (toListOf)
import Control.Monad
import Codec.Picture (Pixel8, PixelRGB8(..), imageHeight, imagePixels, imageWidth)
import Data.Active
import Graphics.Gloss
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit
import TH.RelativePaths
import qualified Data.List.NonEmpty as NonEmpty
import qualified Language.Haskell.TH.Syntax as TH

import Codec.FFmpeg.Extra (Frame)
import Data.Active.FlipBook (Page, FlipBook)
import GlossActive.TestSupport
import GlossToFlv


main :: IO ()
main = defaultMain tests

mkTests
  :: FilePath
  -> (Int, Int)
  -> Color  -- background color
  -> Int  -- frames per second
  -> Active Picture
  -> TestTree
mkTests basename size bg fps animation = do
  testGroup basename
    [ testCase "matches golden file" $ do
        writeFlv actualFilePath size bg fps animation
        expectedFlv <- readFlvFlipBook expectedFilePath
        actualFlv <- readFlvFlipBook actualFilePath
        when (expectedFlv /= actualFlv) $ do
          assertFailure $ "files differ."
    , testCase "round-trips" $ do
        writeFlv actualFilePath size bg fps animation
        (size2, animation2) <- readFlv actualFilePath
        writeFlv roundtripFilePath size2 bg fps animation2
        actualFlv <- readFlvFlipBook actualFilePath
        roundtripFlv <- readFlvFlipBook roundtripFilePath
        when ( not
             $ areFlipBooksSimilar
                 tolerance
                 actualFlv
                 roundtripFlv
             ) $ do
          assertFailure $ "files differ."
    ]
  where
    tolerance = 2.0  -- average pixel difference; might go as high as 20
                     -- for some pixels, but most are 0, so it averages out.
                     -- don't be afraid to bump this up if a test fails but the
                     -- actual and roundtrip files look similar to you; flv is
                     -- a lossy encoding so there could be a lot of artifacts.
    test = $(TH.lift =<< pathRelativeToCabalPackage "test")
    expectedFilePath  = test </> "expected"  </> basename <.> "flv"
    actualFilePath    = test </> "actual"    </> basename <.> "flv"
    roundtripFilePath = test </> "roundtrip" </> basename <.> "flv"

tests
  :: TestTree
tests
  = testGroup "gloss-to-flv Tests"
  [ mkTests "pulsating-circle" (300, 300) white 50 pulsatingCircle
  ]

areFlipBooksSimilar
  :: Double  -- ^ tolerance
  -> FlipBook Frame
  -> FlipBook Frame
  -> Bool
areFlipBooksSimilar tolerance flipBook1 flipBook2
  = and
  $ NonEmpty.zipWith
      (arePagesSimilar tolerance)
      flipBook1
      flipBook2

arePagesSimilar
  :: Double  -- ^ tolerance
  -> Page Frame
  -> Page Frame
  -> Bool
arePagesSimilar tolerance
    (duration1, frame1)
    (duration2, frame2)
  = duration1 == duration2
 && areFramesSimilar tolerance frame1 frame2

areFramesSimilar
  :: Double  -- ^ tolerance
  -> Frame
  -> Frame
  -> Bool
areFramesSimilar tolerance frame1 frame2
  = imageWidth frame1 == imageWidth frame2
 && imageHeight frame1 == imageHeight frame2
 && diffFrames frame1 frame2 <= tolerance

diffFrames
  :: Frame
  -> Frame
  -> Double
diffFrames frame1 frame2
  = sum (zipWith
           diffPixels
           (toListOf imagePixels frame1)
           (toListOf imagePixels frame2))
  / fromIntegral (imageWidth frame1 * imageHeight frame2)

diffPixels
  :: PixelRGB8
  -> PixelRGB8
  -> Double
diffPixels
    (PixelRGB8 r1 g1 b1)
    (PixelRGB8 r2 g2 b2)
  = sum (zipWith
           diffComponents
           [r1,g1,b1]
           [r2,g2,b2])
  / 3

diffComponents
  :: Pixel8
  -> Pixel8
  -> Double
diffComponents x y
  -- be careful to avoid over/underflows!
  = fromIntegral (max x y - min x y)
