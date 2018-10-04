-- | 'Control.Concurrent.ReadWriteLock' reimport, lifted.

module Pos.Util.Concurrent.RWLock
       ( RWLock
       , new
       , acquireRead
       , releaseRead
       , acquireWrite
       , releaseWrite
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
