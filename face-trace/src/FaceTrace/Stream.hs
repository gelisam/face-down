{-# LANGUAGE DeriveFunctor, TemplateHaskell #-}
module FaceTrace.Stream where

import Codec.FFmpeg
import Control.Lens

import Control.Lens.SetterM
import FaceTrace.Frame
import FaceTrace.ReloadableRef
import FaceTrace.Types
import FaceTrace.VideoInfo


data Stream a = Stream
  { _streamWidth         :: Int
  , _streamHeight        :: Int
  , _streamReloadableRef :: ReloadableRef (IO (Maybe (a, Timestamp)))
  }
  deriving Functor

makeLenses ''Stream

streamFrame :: SetterM IO (Stream a) (Stream b) a b
streamFrame = (streamReloadableRef . unSetterM reloadableRefValue) `thenSetterM` monadicSetter `setterMThen` (_Just . _1)


nextFrameTime :: Stream a -> IO (Maybe (a, Timestamp))
nextFrameTime stream = do
  action <- readReloadableRef (stream ^. streamReloadableRef)
  action

nextFrame :: Stream a -> IO (Maybe a)
nextFrame stream = nextFrameTime stream
                 & mapped . _Just %~ fst


withFrameStream :: FilePath -> (Stream Frame -> IO a) -> IO a
withFrameStream filePath body = do
  (pixelWidth, pixelHeight) <- videoDimentions filePath
  withReloadableRef' (imageReaderTime (File filePath)) $ \reloadableRef -> do
    body $ Stream pixelWidth pixelHeight reloadableRef
