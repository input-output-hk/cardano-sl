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
       , getSystemStartReal
       , getSlottingDataReal
       , waitPenultEpochEqualsReal
       , putSlottingDataReal
       ) where

import           Universum

import           Control.Monad.STM            (retry)
import           Control.Monad.Trans.Identity (IdentityT (..))
import           Data.Coerce                  (coerce)
import qualified Ether
import           Pos.Core.Types               (EpochIndex, Timestamp)

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

type SlotsRealMonad m =
    (MonadSlotting m, MonadIO m)

getSystemStartReal :: SlotsRealMonad m => m Timestamp
getSystemStartReal = askSlottingTimestamp

getSlottingDataReal :: SlotsRealMonad m => m SlottingData
getSlottingDataReal = atomically . readTVar =<< askSlottingVar

waitPenultEpochEqualsReal :: SlotsRealMonad m => EpochIndex -> m ()
waitPenultEpochEqualsReal target = do
    var <- askSlottingVar
    atomically $ do
        penultEpoch <- sdPenultEpoch <$> readTVar var
        when (penultEpoch /= target) retry

putSlottingDataReal :: SlotsRealMonad m => SlottingData -> m ()
putSlottingDataReal sd = do
    var <- askSlottingVar
    atomically $ do
        penultEpoch <- sdPenultEpoch <$> readTVar var
        when (penultEpoch < sdPenultEpoch sd) $ writeTVar var sd

instance
    (SlotsRealMonad m, t ~ IdentityT) =>
         MonadSlotsData (Ether.TaggedTrans SlotsDataRedirectTag t m)
  where
    getSystemStart = getSystemStartReal
    getSlottingData = getSlottingDataReal
    waitPenultEpochEquals = waitPenultEpochEqualsReal
    putSlottingData = putSlottingDataReal
