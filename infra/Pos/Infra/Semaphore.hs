-- | 'BlkSemaphore' primitive used for synchronization of threads in
-- the application.

module Pos.Infra.Semaphore
       ( BlkSemaphore (..)
       , modifyBlkSemaphore
       , modifyBlkSemaphore_
       , withBlkSemaphore
       ) where

import           Universum

import           Control.Monad.Catch (MonadMask)

import           Pos.Core            (HeaderHash)
import           Pos.Util.Concurrent (modifyMVar, modifyMVar_, withMVar)
import           Pos.Util.Util       (HasLens', lensOf)

-- | A simple wrapper over 'MVar' which stores 'HeaderHash' (our
-- current tip) and is taken whenever we want to update GState or
-- other data dependent on GState.
newtype BlkSemaphore = BlkSemaphore
    { unBlkSemaphore :: MVar HeaderHash
    }

-- | Run an action acquiring 'BlkSemaphore' lock. Argument of
-- action is an old tip, result is put as a new tip.
modifyBlkSemaphore ::
       ( MonadIO m
       , MonadMask m
       , MonadReader ctx m
       , HasLens' ctx BlkSemaphore
       )
    => (HeaderHash -> m (HeaderHash, a))
    -> m a
modifyBlkSemaphore action = blkSemaphoreHelper (flip modifyMVar action)

-- | Version of 'modifyBlkSemaphore' which doesn't have any result.
modifyBlkSemaphore_ ::
       ( MonadIO m
       , MonadMask m
       , MonadReader ctx m
       , HasLens' ctx BlkSemaphore
       )
    => (HeaderHash -> m HeaderHash)
    -> m ()
modifyBlkSemaphore_ action = blkSemaphoreHelper (flip modifyMVar_ action)

-- | Run an action acquiring 'BlkSemaphore' lock without modifying tip.
withBlkSemaphore ::
       ( MonadIO m
       , MonadMask m
       , MonadReader ctx m
       , HasLens' ctx BlkSemaphore
       )
    => (HeaderHash -> m a)
    -> m a
withBlkSemaphore action = blkSemaphoreHelper (flip withMVar action)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

blkSemaphoreHelper ::
       (MonadReader ctx m, HasLens' ctx BlkSemaphore)
    => (MVar HeaderHash -> m a)
    -> m a
blkSemaphoreHelper action = do
    BlkSemaphore mvar <- view (lensOf @BlkSemaphore)
    action mvar
