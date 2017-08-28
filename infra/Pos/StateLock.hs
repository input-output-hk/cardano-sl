-- | 'StateLock' primitive used for synchronization of threads in
-- the application.

module Pos.StateLock
       ( StateLock (..)
       , modifyStateLock
       , modifyStateLock_
       , withStateLock
       ) where

import           Universum

import           Control.Monad.Catch (MonadMask)

import           Pos.Core            (HeaderHash)
import           Pos.Util.Concurrent (modifyMVar, modifyMVar_, withMVar)
import           Pos.Util.Util       (HasLens', lensOf)

-- | A simple wrapper over 'MVar' which stores 'HeaderHash' (our
-- current tip) and is taken whenever we want to update GState or
-- other data dependent on GState.
newtype StateLock = StateLock
    { unStateLock :: MVar HeaderHash
    }

-- | Run an action acquiring 'StateLock' lock. Argument of
-- action is an old tip, result is put as a new tip.
modifyStateLock ::
       ( MonadIO m
       , MonadMask m
       , MonadReader ctx m
       , HasLens' ctx StateLock
       )
    => (HeaderHash -> m (HeaderHash, a))
    -> m a
modifyStateLock action = blkSemaphoreHelper (flip modifyMVar action)

-- | Version of 'modifyStateLock' which doesn't have any result.
modifyStateLock_ ::
       ( MonadIO m
       , MonadMask m
       , MonadReader ctx m
       , HasLens' ctx StateLock
       )
    => (HeaderHash -> m HeaderHash)
    -> m ()
modifyStateLock_ action = blkSemaphoreHelper (flip modifyMVar_ action)

-- | Run an action acquiring 'StateLock' lock without modifying tip.
withStateLock ::
       ( MonadIO m
       , MonadMask m
       , MonadReader ctx m
       , HasLens' ctx StateLock
       )
    => (HeaderHash -> m a)
    -> m a
withStateLock action = blkSemaphoreHelper (flip withMVar action)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

blkSemaphoreHelper ::
       (MonadReader ctx m, HasLens' ctx StateLock)
    => (MVar HeaderHash -> m a)
    -> m a
blkSemaphoreHelper action = do
    StateLock mvar <- view (lensOf @StateLock)
    action mvar
