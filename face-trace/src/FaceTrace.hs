{-# LANGUAGE LambdaCase, TemplateHaskell #-}
module FaceTrace where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State (StateT, execStateT, get)
import Control.Monad.IO.Class
import Control.Monad.Morph
import Data.Acid (AcidState)
import Data.Monoid
import Graphics.Gloss.Interface.IO.Game
import System.Directory
import System.Exit
import System.FilePath
import qualified Data.Acid as Acid

import FaceTrace.FaceState
import FaceTrace.Size
import FaceTrace.Types
import qualified FaceTrace.FaceMarker  as FaceMarker
import qualified FaceTrace.VideoPlayer as VideoPlayer


data Env = Env
  { _acidState        :: AcidState FaceState
  , _acidStateArchive :: FilePath
  , _size             :: Size
  , _envFaceMarker    :: FaceMarker.Env
  , _envVideoPlayer   :: VideoPlayer.Env
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

withFaceMarker :: ReaderT FaceMarker.Env (StateT FaceMarker.FullState IO) a
               -> ReaderT Env (StateT State IO) a
withFaceMarker = magnify envFaceMarker . zoom stateFaceMarker

withVideoPlayer :: ReaderT VideoPlayer.Env (StateT VideoPlayer.FullState IO) a
                -> ReaderT Env (StateT State IO) a
withVideoPlayer = magnify envVideoPlayer . zoom stateVideoPlayer


initEnv :: FilePath -> IO Env
initEnv filePath = do
  let acidStateDir      = filePath ++ ".face-trace"
  let acidStateArchive_ = acidStateDir </> "Archive"
  acidState_ <- liftIO
              $ Acid.openLocalStateFrom acidStateDir
                                        mempty
  size_ <- videoSize filePath
  Env <$> pure acidState_
      <*> pure acidStateArchive_
      <*> pure size_
      <*> FaceMarker.initEnv acidState_ size_
      <*> VideoPlayer.initEnv size_ filePath

initState :: ReaderT Env IO State
initState = State <$> pure 0
                  <*> (magnify envFaceMarker  $ FaceMarker.initState  ())
                  <*> (magnify envVideoPlayer $ VideoPlayer.initState ())

quit :: ReaderT Env IO ()
quit = do
  env <- ask
  magnify envFaceMarker  FaceMarker.quit
  magnify envVideoPlayer VideoPlayer.quit
  liftIO $ Acid.createCheckpoint (env ^. acidState)
  liftIO $ Acid.createArchive    (env ^. acidState)
  liftIO $ Acid.closeAcidState   (env ^. acidState)
  liftIO $ removeDirectoryRecursive (env ^. acidStateArchive)
  liftIO $ exitSuccess


isFaceMarkerEnabled :: State -> Bool
isFaceMarkerEnabled state = not (state ^. stateVideoPlayer . VideoPlayer.playing)
                         && state ^. timestamp == nearestFaceTimestamp (state ^. timestamp)


draw :: State -> ReaderT Env IO Picture
draw state = do
  videoPlayer <- magnify envVideoPlayer $ VideoPlayer.draw (state ^. stateVideoPlayer)
  faceMarker <- if isFaceMarkerEnabled state
                then magnify envFaceMarker  $ FaceMarker.draw  (state ^. stateFaceMarker)
                else pure blank
  pure (videoPlayer <> faceMarker)



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

overwriteFacePosition :: ReaderT Env (StateT State IO) ()
overwriteFacePosition = do
  state <- lift get
  when (isFaceMarkerEnabled state) $ do
    withFaceMarker FaceMarker.overwriteFacePosition

setMouseCoord :: Coord -> ReaderT Env (StateT State IO) ()
setMouseCoord = withFaceMarker . FaceMarker.setMouseCoord

togglePlaying :: ReaderT Env (StateT State IO) ()
togglePlaying = withVideoPlayer VideoPlayer.toggle


react :: Event -> ReaderT Env (StateT State IO) ()
react (EventKey (SpecialKey KeyEsc)      Down _ _) = hoist lift quit
react (EventKey (SpecialKey KeyLeft)     Down _ _) = moveBackwards
react (EventKey (SpecialKey KeyRight)    Down _ _) = moveForwards
react (EventKey (SpecialKey KeySpace)    Down _ _) = togglePlaying
react (EventKey (MouseButton LeftButton) Down _ _) = overwriteFacePosition
react (EventMotion mouseCoord)                     = setMouseCoord mouseCoord
react _                                            = pure ()

update :: Seconds -> ReaderT Env (StateT State IO) ()
update dt = do
  withFaceMarker  $ FaceMarker.update  dt
  withVideoPlayer $ VideoPlayer.update dt


runApp :: String -> FilePath -> IO ()
runApp windowTitle filePath = do
  env <- initEnv filePath
  initialState <- flip runReaderT env $ initState

  playIO (sizedDisplay windowTitle (env ^. size))
         black
         30
         initialState
         (\state -> flip runReaderT env $ draw state)
         (\event -> execStateT $ flip runReaderT env $ react event)
         (\dt    -> execStateT $ flip runReaderT env $ update $ realToFrac dt)
