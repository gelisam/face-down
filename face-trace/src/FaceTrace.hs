{-# LANGUAGE LambdaCase, TemplateHaskell #-}
module FaceTrace where

import Control.Applicative
import Control.Lens
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State (StateT, execStateT, get)
import Control.Monad.IO.Class
import Control.Monad.Morph
import Data.Monoid
import Graphics.Gloss.Interface.IO.Game
import System.Exit

import FaceTrace.Size
import FaceTrace.Types
import qualified FaceTrace.FaceMarker  as FaceMarker
import qualified FaceTrace.VideoPlayer as VideoPlayer


data Env = Env
  { _envSize        :: Size
  , _envFaceMarker  :: FaceMarker.Env
  , _envVideoPlayer :: VideoPlayer.Env
  }
makeLenses ''Env

data State = State
  { _timestamp               :: Timestamp
  , _partialStateFaceMarker  :: FaceMarker.State ()
  , _partialStateVideoPlayer :: VideoPlayer.State ()
  }
makeLenses ''State

stateFaceMarker :: Lens' State FaceMarker.FullState
stateFaceMarker = lens get_ set_
  where
    get_ :: State -> FaceMarker.FullState
    get_ ss = ss ^. partialStateFaceMarker
            & FaceMarker.timestamp .~ (ss ^. timestamp)

    set_ :: State -> FaceMarker.FullState -> State
    set_ ss s = ss
              & timestamp              .~ (s ^. FaceMarker.timestamp)
              & partialStateFaceMarker .~ (s & FaceMarker.timestamp .~ ())

stateVideoPlayer :: Lens' State VideoPlayer.FullState
stateVideoPlayer = lens get_ set_
  where
    get_ :: State -> VideoPlayer.FullState
    get_ ss = ss ^. partialStateVideoPlayer
            & VideoPlayer.timestamp .~ (ss ^. timestamp)

    set_ :: State -> VideoPlayer.FullState -> State
    set_ ss s = ss
              & timestamp               .~ (s ^. VideoPlayer.timestamp)
              & partialStateVideoPlayer .~ (s & VideoPlayer.timestamp .~ ())


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
draw state = (magnify envVideoPlayer $ VideoPlayer.draw (state ^. stateVideoPlayer))
        <<>> (magnify envFaceMarker  $ FaceMarker.draw  (state ^. stateFaceMarker))
  where
    (<<>>) = liftA2 (<>)


withFaceMarker :: ReaderT FaceMarker.Env (StateT FaceMarker.FullState IO) a
               -> ReaderT Env (StateT State IO) a
withFaceMarker = magnify envFaceMarker . zoom stateFaceMarker

withVideoPlayer :: ReaderT VideoPlayer.Env (StateT VideoPlayer.FullState IO) a
                -> ReaderT Env (StateT State IO) a
withVideoPlayer = magnify envVideoPlayer . zoom stateVideoPlayer

moveBackwards :: ReaderT Env (StateT State IO) ()
moveBackwards = do
  state <- lift get
  if state ^. stateVideoPlayer . VideoPlayer.playing
  then withVideoPlayer VideoPlayer.moveBackwards
  else withFaceMarker FaceMarker.moveBackwards

moveForwards :: ReaderT Env (StateT State IO) ()
moveForwards = do
  state <- lift get
  if state ^. stateVideoPlayer . VideoPlayer.playing
  then withVideoPlayer VideoPlayer.moveForwards
  else withFaceMarker FaceMarker.moveForwards

setMousePos :: Coord -> ReaderT Env (StateT State IO) ()
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

update :: Seconds -> ReaderT Env (StateT State IO) ()
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
         (\dt    -> execStateT $ flip runReaderT env $ update $ realToFrac dt)
