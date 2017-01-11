{-# LANGUAGE ScopedTypeVariables #-}

-- | Functions operating on NodeContext.

module Pos.Context.Functions
       (
         -- * Genesis
         genesisUtxoM
       , genesisLeadersM

         -- * Block semaphore.
       , putBlkSemaphore
       , readBlkSemaphore
       , takeBlkSemaphore

       -- * LRC synchronization
       , waitLrc
       , lrcActionOnEpoch
       , lrcActionOnEpochReason
       ) where

import           Control.Concurrent.MVar (putMVar)
import           Universum

import           Pos.Context.Class       (WithNodeContext (..))
import           Pos.Context.Context     (NodeContext (..))
import           Pos.Lrc.Error           (LrcError (..))
import           Pos.Types               (EpochIndex, HeaderHash, SlotLeaders, Utxo)
import           Pos.Util                (maybeThrow, readTVarConditional)

----------------------------------------------------------------------------
-- Genesis
----------------------------------------------------------------------------

genesisUtxoM :: (Functor m, WithNodeContext ssc m) => m Utxo
genesisUtxoM = ncGenesisUtxo <$> getNodeContext

genesisLeadersM :: (Functor m, WithNodeContext ssc m) => m SlotLeaders
genesisLeadersM = ncGenesisLeaders <$> getNodeContext

----------------------------------------------------------------------------
-- Semaphore-related logic
----------------------------------------------------------------------------

takeBlkSemaphore
    :: (MonadIO m, WithNodeContext ssc m)
    => m (HeaderHash ssc)
takeBlkSemaphore = liftIO . takeMVar . ncBlkSemaphore =<< getNodeContext

putBlkSemaphore
    :: (MonadIO m, WithNodeContext ssc m)
    => HeaderHash ssc -> m ()
putBlkSemaphore tip = liftIO . flip putMVar tip . ncBlkSemaphore =<< getNodeContext

readBlkSemaphore
    :: (MonadIO m, WithNodeContext ssc m)
    => m (HeaderHash ssc)
readBlkSemaphore = liftIO . readMVar . ncBlkSemaphore =<< getNodeContext

----------------------------------------------------------------------------
-- LRC synchronization
----------------------------------------------------------------------------

-- | Block until LRC data is available for given epoch.
waitLrc
    :: (MonadIO m, WithNodeContext ssc m)
    => EpochIndex -> m ()
waitLrc epoch = do
    sync <- ncLrcSync <$> getNodeContext
    () <$ readTVarConditional ((>= epoch) . snd) sync

lrcActionOnEpoch
    :: (MonadIO m, WithNodeContext ssc m, MonadThrow m)
    => EpochIndex
    -> (EpochIndex -> m (Maybe a))
    -> m a
lrcActionOnEpoch epoch =
    lrcActionOnEpochReason
        epoch
        "action on lrcCallOnEpoch couldn't be performed properly"

lrcActionOnEpochReason
    :: (MonadIO m, WithNodeContext ssc m, MonadThrow m)
    => EpochIndex
    -> Text
    -> (EpochIndex -> m (Maybe a))
    -> m a
lrcActionOnEpochReason epoch reason actionDependsOnLrc = do
    waitLrc epoch
    actionDependsOnLrc epoch >>= maybeThrow (LrcDataUnknown epoch reason)
