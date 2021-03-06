{-# LANGUAGE DeriveFunctor, ScopedTypeVariables, TemplateHaskell #-}
module FaceTrace.ReloadableRef where

import Control.Exception
import Control.Lens
import Control.Monad
import Data.IORef

import Control.Lens.SetterM


data ReloadableRef a = ReloadableRef
  { _reloadableRefDeref  :: IO a
  , _reloadableRefReload :: IO ()
  }
  deriving Functor

makeLenses ''ReloadableRef

reloadableRefValue :: SetterM IO (ReloadableRef a) (ReloadableRef b) a b
reloadableRefValue = reloadableRefDeref `thenSetterM` monadicSetter


readReloadableRef :: ReloadableRef a -> IO a
readReloadableRef = view reloadableRefDeref

reloadReloadableRef :: ReloadableRef a -> IO ()
reloadReloadableRef = view reloadableRefReload


withReloadableRef :: forall a r
                   . IO a -> (a -> IO ()) -> (ReloadableRef a -> IO r) -> IO r
withReloadableRef load unload body = bracket (load >>= newIORef)
                                             (readIORef >=> unload)
                                   $ \ref -> do
  body $ ReloadableRef
    { _reloadableRefDeref = readIORef ref
    , _reloadableRefReload = do
        readIORef ref >>= unload
        load >>= atomicWriteIORef ref
    }

-- | A version of 'withReloadableRef' in which the loading action produces both
-- the loaded value and an action to unload it.
withReloadableRef' :: forall a r
                    . IO (a, IO ()) -> (ReloadableRef a -> IO r) -> IO r
withReloadableRef' load body = withReloadableRef load snd (body . fmap fst)
