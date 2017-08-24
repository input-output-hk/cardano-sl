-- | 'BlkSemaphore' primitive used for synchronization of threads in
-- the application.

module Pos.Infra.Semaphore
       ( BlkSemaphore (..)
       , withBlkSemaphore
       , withBlkSemaphoreIgnoreTip
       , withBlkSemaphore_
       ) where

import           Universum

import           Control.Monad.Catch (MonadMask, bracketOnError)

import           Pos.Core            (HeaderHash)
import           Pos.Util.Util       (HasLens, lensOf)

-- | A simple wrapper over 'MVar' which stores 'HeaderHash' (our
-- current tip) and is taken whenever we want to update GState or
-- other data dependent on GState.
newtype BlkSemaphore = BlkSemaphore
    { unBlkSemaphore :: MVar HeaderHash
    }

-- TODO: use 'withMVar' instead ('MonadThrow'-based).
-- | Run action acquiring 'BlkSemaphore' lock. Argument of
-- action is an old tip, result is put as a new tip.
withBlkSemaphore ::
       ( MonadIO m
       , MonadMask m
       , MonadReader ctx m
       , HasLens BlkSemaphore ctx BlkSemaphore
       )
    => (HeaderHash -> m (a, HeaderHash))
    -> m a
withBlkSemaphore action =
    bracketOnError takeBlkSemaphore putBlkSemaphore doAction
  where
    doAction tip = do
        (res, newTip) <- action tip
        res <$ putBlkSemaphore newTip

-- | Version of 'withBlkSemaphore' which doesn't modify tip.
withBlkSemaphoreIgnoreTip ::
       ( MonadIO m
       , MonadMask m
       , MonadReader ctx m
       , HasLens BlkSemaphore ctx BlkSemaphore
       )
    => m a
    -> m a
withBlkSemaphoreIgnoreTip action = withBlkSemaphore wrappedAction
  where
    wrappedAction tip = (, tip) <$> action

-- | Version of 'withBlkSemaphore' which doesn't have any result.
withBlkSemaphore_ ::
       ( MonadIO m
       , MonadMask m
       , MonadReader ctx m
       , HasLens BlkSemaphore ctx BlkSemaphore
       )
    => (HeaderHash -> m HeaderHash)
    -> m ()
withBlkSemaphore_ = withBlkSemaphore . (fmap pure .)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

takeBlkSemaphore
    :: (MonadIO m, MonadReader ctx m, HasLens BlkSemaphore ctx BlkSemaphore)
    => m HeaderHash
takeBlkSemaphore = takeMVar . unBlkSemaphore =<< view (lensOf @BlkSemaphore)

putBlkSemaphore
    :: (MonadIO m, MonadReader ctx m, HasLens BlkSemaphore ctx BlkSemaphore)
    => HeaderHash -> m ()
putBlkSemaphore tip =
    flip putMVar tip . unBlkSemaphore =<< view (lensOf @BlkSemaphore)
