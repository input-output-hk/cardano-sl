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
import           Pos.Slotting.Types           (SlottingData (sdPenultEpoch))

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
    getSlottingData = atomically . readTVar =<< askSlottingVar
    waitPenultEpochEquals target = do
        var <- askSlottingVar
        atomically $ do
            penultEpoch <- sdPenultEpoch <$> readTVar var
            when (penultEpoch /= target) retry
    putSlottingData sd = do
        var <- askSlottingVar
        atomically $ do
            penultEpoch <- sdPenultEpoch <$> readTVar var
            when (penultEpoch < sdPenultEpoch sd) $ writeTVar var sd
