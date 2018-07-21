{-# LANGUAGE LambdaCase, TemplateHaskell #-}
module FaceTrace.VideoPlayer where

import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State (StateT, get)
import Graphics.Gloss.Interface.IO.Game

import FaceTrace.Frame (Frame)
import FaceTrace.Graphics
import FaceTrace.Size
import FaceTrace.Types
import FaceTrace.VideoLoader


data Env = Env
  { _size        :: Size
  , _videoLoader :: VideoLoader Frame
  }
makeLenses ''Env

data State t = State
  { _playing   :: Bool
  , _timestamp :: t
  }
type FullState = State Timestamp
makeLenses ''State


initEnv :: Size -> VideoLoader Frame -> Env
initEnv = Env

initState :: t -> ReaderT Env IO (State t)
initState t = pure $ State False t


draw :: FullState -> ReaderT Env IO Picture
draw state = do
  env <- ask
  liftIO $ atomically $ setPlayTime (env ^. videoLoader) (state ^. timestamp)
  (liftIO $ atomically $ getPlayFrame (env ^. videoLoader)) >>= \case
    Nothing           -> magnify size $ color white <$> textPicture "Loading..."
    Just Nothing      -> magnify size $ color white <$> textPicture "Done!"
    Just (Just frame) -> magnify size $ framePicture frame


moveBackwards :: ReaderT Env (StateT FullState IO) ()
moveBackwards = do
  timestamp -= 2
  timestamp %= max 0

moveForwards :: ReaderT Env (StateT FullState IO) ()
moveForwards = timestamp += 2

pause :: ReaderT Env (StateT FullState IO) ()
pause = playing .= False

play :: ReaderT Env (StateT FullState IO) ()
play = playing .= True

toggle :: ReaderT Env (StateT FullState IO) ()
toggle = playing %= not


update :: Seconds -> ReaderT Env (StateT FullState IO) ()
update dt = do
  state <- lift get
  when (state ^. playing) $ do
    timestamp += dt
