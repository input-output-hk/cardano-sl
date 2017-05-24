-- | 'Control.Corcurrenc.ReadWriteLock' reimport, lifted.

module Pos.Util.Concurrent.RWLock
       ( RWLock
       , new
       , withRead
       , withWrite
       ) where

import           Universum

import           Control.Concurrent.ReadWriteLock (RWLock, acquireRead, acquireWrite,
                                                   releaseRead, releaseWrite)

-- | Create a new 'RWLock' in a free state.
new :: MonadIO m => m RWLock
new = liftIO new

-- | Allows to perform action under shared lock on 'RWLock'.
withRead :: (MonadIO m, MonadMask m) => RWLock -> m a -> m a
withRead l = bracket_ (liftIO $ acquireRead l) (liftIO $ releaseRead l)

-- | Allows to perform action under exclusive lock on 'RWLock'.
withWrite :: (MonadIO m, MonadMask m) => RWLock -> m a -> m a
withWrite l = bracket_ (liftIO $ acquireWrite l) (liftIO $ releaseWrite l)
