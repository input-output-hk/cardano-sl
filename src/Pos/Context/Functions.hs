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

       -- * NTP
       , setNtpLastSlot
       , readNtpLastSlot
       , readNtpMargin
       , readNtpData
       ) where

import           Control.Concurrent.MVar (putMVar)
import qualified Control.Concurrent.STM  as STM
import           Data.Time.Units         (Microsecond)
import           Universum

import           Pos.Context.Class       (WithNodeContext (..))
import           Pos.Context.Context     (NodeContext (..))
import           Pos.Lrc.Error           (LrcError (..))
import           Pos.Slotting            (ssNtpData, ssNtpLastSlot)
import           Pos.Types               (EpochIndex, HeaderHash, SlotId, SlotLeaders,
                                          Utxo)
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
    => m HeaderHash
takeBlkSemaphore = liftIO . takeMVar . ncBlkSemaphore =<< getNodeContext

putBlkSemaphore
    :: (MonadIO m, WithNodeContext ssc m)
    => HeaderHash -> m ()
putBlkSemaphore tip = liftIO . flip putMVar tip . ncBlkSemaphore =<< getNodeContext

readBlkSemaphore
    :: (MonadIO m, WithNodeContext ssc m)
    => m HeaderHash
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

----------------------------------------------------------------------------
-- NTP data
----------------------------------------------------------------------------
setNtpLastSlot :: (MonadIO m, WithNodeContext ssc m) => SlotId -> m ()
setNtpLastSlot slotId = do
    nc <- getNodeContext
    atomically $ STM.modifyTVar (ncSlottingState nc)
                                (ssNtpLastSlot %~ max slotId)

readNtpLastSlot :: (MonadIO m, WithNodeContext ssc m) => m SlotId
readNtpLastSlot = do
    nc <- getNodeContext
    atomically $ view ssNtpLastSlot <$> STM.readTVar (ncSlottingState nc)

readNtpMargin :: (MonadIO m, WithNodeContext ssc m) => m Microsecond
readNtpMargin = do
    nc <- getNodeContext
    atomically $ fst . view ssNtpData <$> STM.readTVar (ncSlottingState nc)

readNtpData
    :: (MonadIO m, WithNodeContext ssc m)
    => m (Microsecond, Microsecond)
readNtpData = do
    nc <- getNodeContext
    atomically $ view ssNtpData <$> STM.readTVar (ncSlottingState nc)
