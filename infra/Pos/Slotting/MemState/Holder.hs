{-# LANGUAGE TypeFamilies #-}

-- | Default implementation of 'MonadSlotsData' based on 'TVar'.

module Pos.Slotting.MemState.Holder
       ( SlottingVar
       , MonadSlotting
       , askSlotting
       , askSlottingVar
       , askSlottingTimestamp
       , SlotsDataRedirect
       , runSlotsDataRedirect
       ) where

import           Universum

import           Control.Monad.STM            (retry)
import           Control.Monad.Trans.Identity (IdentityT (..))
import           Data.Coerce                  (coerce)
import qualified Ether
import           Pos.Core.Types               (Timestamp)

import           Pos.Slotting.MemState.Class  (MonadSlotsData (..))
import           Pos.Slotting.Types           (SlottingData, addEpochSlottingData,
                                               getPenultEpochIndex, lookupEpochSlottingData, getLastEpochIndex)

----------------------------------------------------------------------------
-- Transformer
----------------------------------------------------------------------------

-- | System start and slotting data
type SlottingVar = (Timestamp, TVar SlottingData)

type MonadSlotting = Ether.MonadReader' SlottingVar

askSlotting :: MonadSlotting m => m SlottingVar
askSlotting = Ether.ask'

askSlottingVar :: MonadSlotting m => m (TVar SlottingData)
askSlottingVar = snd <$> askSlotting

askSlottingTimestamp :: MonadSlotting m => m Timestamp
askSlottingTimestamp  = fst <$> askSlotting

----------------------------------------------------------------------------
-- MonadSlotsData implementation
----------------------------------------------------------------------------

data SlotsDataRedirectTag

type SlotsDataRedirect =
    Ether.TaggedTrans SlotsDataRedirectTag IdentityT

runSlotsDataRedirect :: SlotsDataRedirect m a -> m a
runSlotsDataRedirect = coerce

instance
    (MonadSlotting m, MonadIO m, t ~ IdentityT) =>
         MonadSlotsData (Ether.TaggedTrans SlotsDataRedirectTag t m)
  where
    getSystemStart = askSlottingTimestamp
    getEpochLastIndex = do
        var <- askSlottingVar
        atomically $ (fromMaybe 0 . getLastEpochIndex) <$> readTVar var
    getEpochSlottingData ei = do
        var <- askSlottingVar
        atomically $ lookupEpochSlottingData ei <$> readTVar var
    waitPenultEpochEquals target = do
        var <- askSlottingVar
        atomically $ do
            penultEpoch <- getPenultEpochIndex <$> readTVar var
            when (penultEpoch /= target) retry
    putEpochSlottingData ei esd = do
        var <- askSlottingVar
        atomically $ do
            sd <- readTVar var
            writeTVar var (addEpochSlottingData ei esd sd)
