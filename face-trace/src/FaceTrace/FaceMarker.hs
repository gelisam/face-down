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
import Linear
import "extra" Control.Monad.Extra (whenJust)
import qualified Data.Acid as Acid

import FaceTrace.Graphics
import FaceTrace.FaceState
import FaceTrace.Interpolate
import FaceTrace.Coord
import FaceTrace.Rel
import FaceTrace.Size
import FaceTrace.Types


data Env = Env
  { _acidState   :: AcidState FaceState
  , _displaySize :: Size
  }
makeLenses ''Env

data State t = State
  { _relFacePositions :: Map FaceTimestamp Rel
  , _mouseCoord       :: Maybe Coord
  , _timestamp        :: t
  }
type FullState = State Timestamp
makeLenses ''State


initEnv :: AcidState FaceState -> Size -> IO Env
initEnv acidState_ displaySize_ = pure $ Env acidState_ displaySize_

initState :: t -> ReaderT Env IO (State t)
initState t = do
  env <- ask
  facePositions <- liftIO $ Acid.query (env ^. acidState) GetFacePositions
  pure $ State (fromFacePosition <$> facePositions) Nothing t


drawAt :: Maybe Coord -> ReaderT Env IO Picture
drawAt maybeCoord = magnify displaySize $ do
  color (makeColor 0 0 0 0.5) <$> case maybeCoord of
    Just (Coord (V2 x y)) -> antiRectangle 200 150
                         <&> translate x y
    Nothing               -> clear

draw :: FullState -> ReaderT Env IO Picture
draw state = do
  coord <- case state ^. relFacePositions . at (state ^. timestamp) of
    Just rel -> magnify displaySize $ Just <$> toCoord rel
    Nothing  -> pure (state ^. mouseCoord)
  drawAt coord

drawInterpolated :: FullState -> ReaderT Env IO Picture
drawInterpolated state = do
  case followFace (state ^. relFacePositions) (state ^. timestamp) of
    Just rel -> do
      coord <- magnify displaySize $ toCoord rel
      drawAt $ Just coord
    Nothing -> pure blank


overwriteFacePosition :: ReaderT Env (StateT FullState IO) ()
overwriteFacePosition = do
  env <- ask
  state <- lift get
  whenJust (state ^. mouseCoord) $ \coord -> do
    rel <- magnify displaySize $ toRel coord
    relFacePositions . at (state ^. timestamp) .= Just rel
    let t       = state ^. timestamp
        facePos = toFacePosition rel
    liftIO $ Acid.update (env ^. acidState) $ InsertFacePosition t facePos

saveFacePosition :: ReaderT Env (StateT FullState IO) ()
saveFacePosition = do
  state <- lift get
  case (state ^. relFacePositions . at (state ^. timestamp)) of
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
    let ww = env ^. displaySize . sizeWidth
    let hh = env ^. displaySize . sizeHeight
    guard $ pointInBox (toPoint coord) (-ww / 2, -hh / 2)
                                       ( ww / 2,  hh / 2)
    pure coord


update :: Seconds -> ReaderT Env (StateT FullState IO) ()
update _ = pure ()
