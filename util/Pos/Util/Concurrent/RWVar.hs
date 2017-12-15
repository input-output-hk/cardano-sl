-- | Reimplementation of 'Control.Corcurrenc.ReadWriteVar', lifted and
-- adopted to 'MonadMask'.
--
-- License    : BSD3 (see the file LICENSE)
-- Copyright  : 2010—2011 Bas van Dijk & Roel van Dijk

module Pos.Util.Concurrent.RWVar
       ( RWVar
       , new
       , with
       , read
       , modify
       , modifyPure
       ) where

import           Universum hiding (modify)

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
new = liftA2 RWVar RWL.new . newIORef

-- | Executes 'MonadIO' action taking a shared lock inside.
with :: (MonadIO m, MonadMask m) => RWVar a -> (a -> m b) -> m b
with (RWVar l r) action = RWL.withRead l $ readIORef r >>= action

-- | Reads the value inside 'RWVar'.
read :: (MonadIO m, MonadMask m) => RWVar a -> m a
read var = with var pure

-- | Executes 'MonadIO' action taking an exclusive lock inside.
modify :: (MonadIO m, MonadMask m) => RWVar a -> (a -> m (a, b)) -> m b
modify (RWVar l r) = RWL.withWrite l . modifyIORefM r

modifyIORefM :: (MonadIO m) => IORef a -> (a -> m (a, b)) -> m b
modifyIORefM r f = do
    (y, z) <- f =<< readIORef r
    writeIORef r y
    pure z

-- | Executes 'MonadIO' action taking an exclusive lock inside.
modifyPure :: (MonadIO m, MonadMask m) => RWVar a -> (a -> a) -> m ()
modifyPure var foo = modify var $ \x -> pure (foo x, ())
