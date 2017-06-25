{-# LANGUAGE TypeFamilies #-}

-- | Default implementation of 'MonadSlotsData' based on 'TVar'.

module Pos.Slotting.MemState.Holder
       ( SlottingVar
       , MonadSlotting
       , askSlotting
       , askSlottingVar
       , askSlottingTimestamp
       , getSystemStartDefault
       , getSlottingDataDefault
       , waitPenultEpochEqualsDefault
       , putSlottingDataDefault
       ) where

import           Universum

import           Control.Monad.STM  (retry)
import           EtherCompat

import           Pos.Core.Types     (EpochIndex, Timestamp)
import           Pos.Slotting.Types (SlottingData (sdPenultEpoch))

----------------------------------------------------------------------------
-- Transformer
----------------------------------------------------------------------------

-- | System start and slotting data
type SlottingVar = (Timestamp, TVar SlottingData)

type MonadSlotting ctx m = MonadCtx ctx SlottingVar SlottingVar m

askSlotting :: MonadSlotting ctx m => m SlottingVar
askSlotting = askCtx @SlottingVar

askSlottingVar :: MonadSlotting ctx m => m (TVar SlottingData)
askSlottingVar = snd <$> askSlotting

askSlottingTimestamp :: MonadSlotting ctx m => m Timestamp
askSlottingTimestamp  = fst <$> askSlotting

----------------------------------------------------------------------------
-- MonadSlotsData implementation
----------------------------------------------------------------------------

type SlotsDefaultEnv ctx m =
    (MonadSlotting ctx m, MonadIO m)

getSystemStartDefault :: SlotsDefaultEnv ctx m => m Timestamp
getSystemStartDefault = askSlottingTimestamp

getSlottingDataDefault :: SlotsDefaultEnv ctx m => m SlottingData
getSlottingDataDefault = atomically . readTVar =<< askSlottingVar

waitPenultEpochEqualsDefault :: SlotsDefaultEnv ctx m => EpochIndex -> m ()
waitPenultEpochEqualsDefault target = do
    var <- askSlottingVar
    atomically $ do
        penultEpoch <- sdPenultEpoch <$> readTVar var
        when (penultEpoch /= target) retry

putSlottingDataDefault :: SlotsDefaultEnv ctx m => SlottingData -> m ()
putSlottingDataDefault sd = do
    var <- askSlottingVar
    atomically $ do
        penultEpoch <- sdPenultEpoch <$> readTVar var
        when (penultEpoch < sdPenultEpoch sd) $ writeTVar var sd
