{-# LANGUAGE LambdaCase, TemplateHaskell #-}
module FaceTrace where

import Control.Applicative
import Control.Lens
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State (StateT, execStateT)
import Control.Monad.IO.Class
import Control.Monad.Morph
import Data.Monoid
import Graphics.Gloss.Interface.IO.Game
import System.Exit

import FaceTrace.Size
import qualified FaceTrace.FaceMarker  as FaceMarker
import qualified FaceTrace.VideoPlayer as VideoPlayer


data Env = Env
  { _envSize        :: Size
  , _envFaceMarker  :: FaceMarker.Env
  , _envVideoPlayer :: VideoPlayer.Env
  }
makeLenses ''Env

data State = State
  { _timestamp        :: Double
  , _stateFaceMarker  :: FaceMarker.State ()
  , _stateVideoPlayer :: VideoPlayer.State ()
  }
makeLenses ''State

fullStateFaceMarker :: Lens' State FaceMarker.FullState
fullStateFaceMarker = lens get_ set_
  where
    get_ :: State -> FaceMarker.FullState
    get_ state = state ^. stateFaceMarker
               & FaceMarker.timestamp .~ (state ^. timestamp)

    set_ :: State -> FaceMarker.FullState -> State
    set_ state fullState = state
                         & timestamp       .~ (fullState ^. FaceMarker.timestamp)
                         & stateFaceMarker .~ (fullState & FaceMarker.timestamp .~ ())

fullStateVideoPlayer :: Lens' State VideoPlayer.FullState
fullStateVideoPlayer = lens get_ set_
  where
    get_ :: State -> VideoPlayer.FullState
    get_ state = state ^. stateVideoPlayer
               & VideoPlayer.timestamp .~ (state ^. timestamp)

    set_ :: State -> VideoPlayer.FullState -> State
    set_ state fullState = state
                         & timestamp       .~ (fullState ^. VideoPlayer.timestamp)
                         & stateVideoPlayer .~ (fullState & VideoPlayer.timestamp .~ ())


initEnv :: FilePath -> IO Env
initEnv filePath = do
  size_ <- videoSize filePath
  Env <$> pure size_
      <*> pure (FaceMarker.initEnv size_)
      <*> VideoPlayer.initEnv size_ filePath

initState :: State
initState = State 0
                  (FaceMarker.initState ())
                  (VideoPlayer.initState ())

quit :: ReaderT Env IO ()
quit = do
  magnify envFaceMarker  FaceMarker.quit
  magnify envVideoPlayer VideoPlayer.quit
  liftIO exitSuccess


draw :: State -> ReaderT Env IO Picture
draw state = (magnify envVideoPlayer $ VideoPlayer.draw (state ^. fullStateVideoPlayer))
        <<>> (magnify envFaceMarker  $ FaceMarker.draw  (state ^. fullStateFaceMarker))
  where
    (<<>>) = liftA2 (<>)


withFaceMarker :: ReaderT FaceMarker.Env (StateT FaceMarker.FullState IO) a
               -> ReaderT Env (StateT State IO) a
withFaceMarker = magnify envFaceMarker . zoom fullStateFaceMarker

withVideoPlayer :: ReaderT VideoPlayer.Env (StateT VideoPlayer.FullState IO) a
                -> ReaderT Env (StateT State IO) a
withVideoPlayer = magnify envVideoPlayer . zoom fullStateVideoPlayer

moveBackwards :: ReaderT Env (StateT State IO) ()
moveBackwards = withVideoPlayer VideoPlayer.moveBackwards

moveForwards :: ReaderT Env (StateT State IO) ()
moveForwards = withVideoPlayer VideoPlayer.moveForwards

setMousePos :: (Float, Float) -> ReaderT Env (StateT State IO) ()
setMousePos = withFaceMarker . FaceMarker.setMousePos

togglePlaying :: ReaderT Env (StateT State IO) ()
togglePlaying = withVideoPlayer VideoPlayer.toggle


react :: Event -> ReaderT Env (StateT State IO) ()
react (EventKey (SpecialKey KeyEsc)   Down _ _) = hoist lift quit
react (EventKey (SpecialKey KeyLeft)  Down _ _) = moveBackwards
react (EventKey (SpecialKey KeyRight) Down _ _) = moveForwards
react (EventKey (SpecialKey KeySpace) Down _ _) = togglePlaying
react (EventMotion mousePos)                    = setMousePos mousePos
react _                                         = pure ()

update :: Float -> ReaderT Env (StateT State IO) ()
update dt = do
  withFaceMarker  $ FaceMarker.update  dt
  withVideoPlayer $ VideoPlayer.update dt


runApp :: String -> FilePath -> IO ()
runApp windowTitle filePath = do
  env <- initEnv filePath
  let initialState = initState

  playIO (sizedDisplay windowTitle (env ^. envSize))
         black
         30
         initialState
         (\state -> flip runReaderT env $ draw state)
         (\event -> execStateT $ flip runReaderT env $ react event)
         (\dt    -> execStateT $ flip runReaderT env $ update dt)
