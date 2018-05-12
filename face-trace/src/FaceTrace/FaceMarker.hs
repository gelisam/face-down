{-# LANGUAGE TemplateHaskell #-}
module FaceTrace.FaceMarker where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State (StateT)
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Interface.IO.Game

import FaceTrace.Graphics
import FaceTrace.FaceState
import FaceTrace.Size
import FaceTrace.Types


data Env = Env
  { _size :: Size
  }
makeLenses ''Env

data State t = State
  { _mouseCoord :: Maybe Coord
  , _timestamp  :: t
  }
type FullState = State Timestamp
makeLenses ''State


initEnv :: Size -> Env
initEnv size_ = Env size_

initState :: t -> State t
initState = State Nothing

quit :: ReaderT Env IO ()
quit = pure ()


draw :: FullState -> ReaderT Env IO Picture
draw state = magnify size $ do
  color (makeColor 0 0 0 0.5) <$> case state ^. mouseCoord of
    Just (x,y) -> antiRectangle 200 150
              <&> translate x y
    Nothing    -> clear


moveBackwards :: ReaderT Env (StateT FullState IO) ()
moveBackwards = timestamp %= previousFaceTimestamp

moveForwards :: ReaderT Env (StateT FullState IO) ()
moveForwards = timestamp %= nextFaceTimestamp

setMouseCoord :: Coord -> ReaderT Env (StateT FullState IO) ()
setMouseCoord coord = do
  env <- ask
  mouseCoord .= do
    let ww = fromIntegral (env ^. size . displayWidth)
    let hh = fromIntegral (env ^. size . displayHeight)
    guard $ pointInBox coord (-ww / 2, -hh / 2)
                             ( ww / 2,  hh / 2)
    pure coord


update :: Seconds -> ReaderT Env (StateT FullState IO) ()
update _ = pure ()
