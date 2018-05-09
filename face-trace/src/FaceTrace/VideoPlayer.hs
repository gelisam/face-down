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

import FaceTrace.Graphics
import FaceTrace.Size
import FaceTrace.VideoLoader


data Env = Env
  { _size        :: Size
  , _videoLoader :: VideoLoader
  }
makeLenses ''Env

data State = State
  { _playing   :: Bool
  , _timestamp :: Double
  }
makeLenses ''State


initEnv :: Size -> FilePath -> IO Env
initEnv size_ filePath = Env size_
                     <$> videoLoaderOpen filePath 2 4

initState :: State
initState = State False 0

quit :: ReaderT Env IO ()
quit = do
  env <- ask
  lift $ videoLoaderClose (env ^. videoLoader)


draw :: State -> ReaderT Env IO Picture
draw state = do
  env <- ask
  liftIO $ atomically $ setPlayTime (env ^. videoLoader) (state ^. timestamp)
  (liftIO $ atomically $ getPlayFrame (env ^. videoLoader)) >>= \case
    Nothing           -> magnify size $ textPicture "Loading..."
    Just Nothing      -> magnify size $ textPicture "Done!"
    Just (Just frame) -> magnify size $ framePicture frame


moveBackwards :: ReaderT Env (StateT State IO) ()
moveBackwards = do
  timestamp -= 2
  timestamp %= max 0

moveForwards :: ReaderT Env (StateT State IO) ()
moveForwards = timestamp += 2

pause :: ReaderT Env (StateT State IO) ()
pause = playing .= False

play :: ReaderT Env (StateT State IO) ()
play = playing .= True

toggle :: ReaderT Env (StateT State IO) ()
toggle = playing %= not


update :: Float -> ReaderT Env (StateT State IO) ()
update dt = do
  state <- lift get
  when (state ^. playing) $ do
    timestamp += realToFrac dt
