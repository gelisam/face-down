{-# LANGUAGE LambdaCase, TemplateHaskell #-}
module FaceTrace.VideoPlayer (videoPlayer) where

import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Data.Maybe
import Data.Monoid
import Data.Ratio
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Interface.IO.Game
import System.Exit

import Control.Monad.Extra
import FaceTrace.Frame
import FaceTrace.VideoInfo
import FaceTrace.VideoLoader
import Graphics.Gloss.Extra


data State = State
  { _statePlaying   :: Bool
  , _stateTimestamp :: Double
  , _stateMousePos  :: Maybe (Float, Float)
  }

makeLenses ''State


videoPlayer :: String -> FilePath -> IO ()
videoPlayer windowTitle filePath = do
  videoLoader <- videoLoaderOpen filePath 2 4
  (pixelWidth, pixelHeight) <- videoDimentions filePath
  pixelAspectRatio <- videoPixelAspectRatio filePath
                  <&> fromMaybe 1
  let displayAspectRatio :: Ratio Int
      displayAspectRatio = (pixelWidth  * numerator pixelAspectRatio)
                         % (pixelHeight * denominator pixelAspectRatio)

      displayWidth :: Int
      displayWidth = div pixelWidth <$> [1..]
                   & dropWhile (> 1024)
                   & head

      displayHeight :: Int
      displayHeight = round (fromIntegral displayWidth / displayAspectRatio)

      display :: Display
      display = InWindow windowTitle
                         (displayWidth, displayHeight)
                         (10, 10)

      scaleX :: Float
      scaleX = fromIntegral displayWidth / fromIntegral pixelWidth

      scaleY :: Float
      scaleY = fromIntegral displayHeight / fromIntegral pixelHeight

      textPicture :: String -> Picture
      textPicture = translate (fromIntegral (-displayWidth) / 2 + 5)
                              (fromIntegral displayHeight   / 2 - 5)
                  . scale 0.15 0.15
                  . translate 0 (-113)
                  . color white
                  . text

      setTimestamp :: Double -> State -> IO State
      setTimestamp t state = do
        atomically $ setPlayTime videoLoader t
        pure $ state
             & stateTimestamp .~ t

      modifyTimestamp :: (Double -> Double) -> State -> IO State
      modifyTimestamp f state = do
        let t = view stateTimestamp state
        setTimestamp (f t) state

      draw :: State -> IO Picture
      draw state = atomically (getPlayFrame videoLoader) <&> \case
        Nothing -> textPicture "Loading..."
        Just Nothing -> textPicture "Done!"
        Just (Just frame) ->
          let videoFrame = frame
                         & framePicture
                         & scale scaleX scaleY
              mouseRect = color (makeColor 0 0 0 0.5)
                        $ case state ^. stateMousePos of
                Nothing    -> rectangleSolid (fromIntegral displayWidth)
                                             (fromIntegral displayHeight)
                Just (x,y) -> antiRectangle (2 * fromIntegral displayWidth)
                                            (2 * fromIntegral displayHeight)
                                            200 150
                            & translate x y
          in videoFrame <> mouseRect

      react :: Event -> State -> IO State
      react (EventKey (SpecialKey KeyEsc)   Down _ _) = \_ -> do
        videoLoaderClose videoLoader
        exitSuccess
      react (EventKey (Char 'r')            Down _ _) = setTimestamp 0
      react (EventKey (SpecialKey KeyLeft)  Down _ _) = modifyTimestamp $ \t
                                                     -> (t - 2) `max` 0
      react (EventKey (SpecialKey KeyRight) Down _ _) = modifyTimestamp (+ 2)
      react (EventKey (SpecialKey KeySpace) Down _ _) = pure
                                                    >&> statePlaying %~ not
      react (EventMotion mousePos)                    = pure
                                                    >&> stateMousePos .~ do
        guard $ pointInBox mousePos (-fromIntegral displayWidth / 2, -fromIntegral displayHeight / 2)
                                    ( fromIntegral displayWidth / 2,  fromIntegral displayHeight / 2)
        pure mousePos
      react _                                         = pure

      update :: Float -> State -> IO State
      update dt state = do
        if state ^. statePlaying
        then do
          atomically (getPlayFrame videoLoader) >>= \case
            Nothing -> pure state
            Just _ -> do
              let t' = state ^. stateTimestamp + realToFrac dt
              atomically $ setPlayTime videoLoader t'
              pure $ state
                   & stateTimestamp .~ t'
        else pure state

  playIO display
         black
         30
         (State False 0 Nothing)
         draw
         react
         update
