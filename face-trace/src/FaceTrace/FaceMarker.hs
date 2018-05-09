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


data Env = Env
  { _size :: Size
  }
makeLenses ''Env

data State t = State
  { _mousePos  :: Maybe (Float, Float)
  , _timestamp :: t
  }
type FullState = State Double
makeLenses ''State


initEnv :: Size -> Env
initEnv size_ = Env size_

initState :: t -> State t
initState = State Nothing

quit :: ReaderT Env IO ()
quit = pure ()


draw :: FullState -> ReaderT Env IO Picture
draw state = magnify size $ do
  color (makeColor 0 0 0 0.5) <$> case state ^. mousePos of
    Just (x,y) -> antiRectangle 200 150
              <&> translate x y
    Nothing    -> clear


moveBackwards :: ReaderT Env (StateT FullState IO) ()
moveBackwards = timestamp %= previousFaceTimestamp

moveForwards :: ReaderT Env (StateT FullState IO) ()
moveForwards = timestamp %= nextFaceTimestamp

setMousePos :: (Float, Float) -> ReaderT Env (StateT FullState IO) ()
setMousePos pos = do
  env <- ask
  mousePos .= do
    let ww = fromIntegral (env ^. size . displayWidth)
    let hh = fromIntegral (env ^. size . displayHeight)
    guard $ pointInBox pos (-ww / 2, -hh / 2)
                           ( ww / 2,  hh / 2)
    pure pos


update :: Float -> ReaderT Env (StateT FullState IO) ()
update _ = pure ()
