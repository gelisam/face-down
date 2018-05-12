{-# LANGUAGE PackageImports, TemplateHaskell #-}
module FaceTrace.FaceMarker where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State (StateT, get)
import Data.Acid (AcidState)
import Data.Map.Strict (Map)
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Interface.IO.Game
import "extra" Control.Monad.Extra (whenJust)
import qualified Data.Acid as Acid

import FaceTrace.Graphics
import FaceTrace.FaceState
import FaceTrace.Size
import FaceTrace.Types


data Env = Env
  { _acidState :: AcidState FaceState
  , _size      :: Size
  }
makeLenses ''Env

data State t = State
  { _facePositions :: Map FaceTimestamp Pos
  , _mouseCoord    :: Maybe Coord
  , _timestamp     :: t
  }
type FullState = State Timestamp
makeLenses ''State


initEnv :: AcidState FaceState -> Size -> IO Env
initEnv acidState_ size_ = pure $ Env acidState_ size_

initState :: t -> ReaderT Env IO (State t)
initState t = do
  env <- ask
  facePositions_ <- liftIO $ Acid.query (env ^. acidState) GetFacePositions
  pure $ State facePositions_ Nothing t

quit :: ReaderT Env IO ()
quit = pure ()


draw :: FullState -> ReaderT Env IO Picture
draw state = magnify size $ do
  coord <- case state ^. facePositions . at (state ^. timestamp) of
    Just pos -> Just <$> toCoord pos
    Nothing  -> pure (state ^. mouseCoord)

  color (makeColor 0 0 0 0.5) <$> case coord of
    Just (x,y) -> antiRectangle 200 150
              <&> translate x y
    Nothing    -> clear


overwriteFacePosition :: ReaderT Env (StateT FullState IO) ()
overwriteFacePosition = do
  env <- ask
  state <- lift get
  whenJust (state ^. mouseCoord) $ \coord -> do
    pos <- magnify size $ toPos coord
    facePositions . at (state ^. timestamp) .= Just pos
    liftIO $ Acid.update (env ^. acidState) $ InsertFacePosition (state ^. timestamp) pos

saveFacePosition :: ReaderT Env (StateT FullState IO) ()
saveFacePosition = do
  state <- lift get
  case (state ^. facePositions . at (state ^. timestamp)) of
    Nothing -> overwriteFacePosition
    Just _  -> pure ()

moveBackwards :: ReaderT Env (StateT FullState IO) ()
moveBackwards = do
  saveFacePosition
  timestamp %= previousFaceTimestamp

moveForwards :: ReaderT Env (StateT FullState IO) ()
moveForwards = do
  saveFacePosition
  timestamp %= nextFaceTimestamp

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
