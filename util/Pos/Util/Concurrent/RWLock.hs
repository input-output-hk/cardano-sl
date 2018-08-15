-- | 'Control.Concurrent.ReadWriteLock' reimport, lifted.

module Pos.Util.Concurrent.RWLock
       ( RWLock
       , new
       , acquireRead
       , releaseRead
       , acquireWrite
       , releaseWrite
       , whenAcquireWrite
       , withRead
       , withWrite
       ) where

import           Universum

import           Control.Concurrent.ReadWriteLock (RWLock)
import qualified Control.Concurrent.ReadWriteLock as RWL

-- | Create a new 'RWLock' in a free state.
new :: MonadIO m => m RWLock
new = liftIO RWL.new

-- | Acquire the read lock
acquireRead :: MonadIO m => RWLock -> m ()
acquireRead = liftIO . RWL.acquireRead

-- | Release the read lock
releaseRead :: MonadIO m => RWLock -> m ()
releaseRead = liftIO . RWL.releaseRead

-- | Acquire the write lock
acquireWrite :: MonadIO m => RWLock -> m ()
acquireWrite = liftIO . RWL.acquireWrite

-- | Release the write lock
releaseWrite :: MonadIO m => RWLock -> m ()
releaseWrite = liftIO . RWL.releaseWrite

-- | Allows to perform action under shared lock on 'RWLock'.
withRead :: (MonadIO m, MonadMask m) => RWLock -> m a -> m a
withRead l = bracket_ (acquireRead l) (releaseRead l)

-- | Allows to perform action under exclusive lock on 'RWLock'.
withWrite :: (MonadIO m, MonadMask m) => RWLock -> m a -> m a
withWrite l = bracket_ (acquireWrite l) (releaseWrite l)

-- | If the write lock can be acquired, perform the requested action
-- and then release the lock. If the lock cannot be acquired, no
-- action is performed.
whenAcquireWrite :: (MonadIO m, MonadMask m) => RWLock -> m a -> m (Maybe a)
whenAcquireWrite l action =
    bracket
        (liftIO $ RWL.tryAcquireWrite l)
        (\haveLock -> when haveLock $ releaseWrite l)
        (\haveLock -> if haveLock then Just <$> action else pure Nothing)
