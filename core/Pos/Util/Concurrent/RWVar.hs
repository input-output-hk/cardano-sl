-- | Reimplementation of 'Control.Corcurrenc.ReadWriteVar', lifted and
-- adopted to 'MonadMask'.
--
-- License    : BSD3 (see the file LICENSE)
-- Copyright  : 2010—2011 Bas van Dijk & Roel van Dijk

module Pos.Util.Concurrent.RWVar
       ( RWVar
       , new
       , with
       , modify
       ) where

import           Universum                  hiding (modify)

import           Pos.Util.Concurrent.RWLock (RWLock)
import qualified Pos.Util.Concurrent.RWLock as RWL

-- | Concurrently readable and sequentially writable variable.
data RWVar a = RWVar RWLock (IORef a) deriving Typeable

instance Eq (RWVar a) where
    (==) = (==) `on` rwlock
        where
          rwlock (RWVar rwl _) = rwl

-- | Creates a new 'RWVar' from given value.
new :: MonadIO m => a -> m (RWVar a)
new = liftIO . new

-- | Executes 'MonadIO' action taking a shared lock inside.
with :: (MonadIO m, MonadMask m) => RWVar a -> (a -> m b) -> m b
with (RWVar l r) action = RWL.withRead l $ liftIO (readIORef r) >>= action

-- | Executes 'MonadIO' action taking an exclusive lock inside.
modify :: (MonadIO m, MonadMask m) => RWVar a -> (a -> m (a, b)) -> m b
modify (RWVar l r) = RWL.withWrite l . modifyIORefM r

modifyIORefM :: (MonadIO m) => IORef a -> (a -> m (a, b)) -> m b
modifyIORefM r f = do
    (y, z) <- f =<< liftIO (readIORef r)
    liftIO $ writeIORef r y
    pure z
