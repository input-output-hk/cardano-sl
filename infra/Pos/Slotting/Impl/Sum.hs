{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Sum of slotting implementations.

module Pos.Slotting.Impl.Sum
       ( SlottingContextSum (..)
       , MonadSlottingSum
       , SlotsRedirect
       , runSlotsRedirect
       , askSlottingContextSum
       , getCurrentSlotSum
       , getCurrentSlotBlockingSum
       , getCurrentSlotInaccurateSum
       , currentTimeSlottingSum

       -- * Workers
       , SlottingWorkerModeSum
       , slottingWorkers
       ) where

import           Universum

import           Control.Monad.Trans.Identity (IdentityT (..))
import           Data.Coerce                  (coerce)
import qualified Ether

import           Pos.Core.Types               (SlotId (..), Timestamp)
import           Pos.Slotting.Class           (MonadSlots (..))
import           Pos.Slotting.Impl.Ntp        (NtpMode, NtpSlottingVar, NtpWorkerMode,
                                               ntpWorkers, runNtpSlotsRedirect)
import           Pos.Slotting.Impl.Simple     (SimpleSlottingMode, runSimpleSlotsRedirect)

-- | Sum of all contexts used by slotting implementations.
data SlottingContextSum
    = SCSimple
    | SCNtp NtpSlottingVar

-- | Monad which combines all 'MonadSlots' implementations (and
-- uses only one of them).
type MonadSlottingSum = Ether.MonadReader' SlottingContextSum

data SlotsRedirectTag

type SlotsRedirect =
    Ether.TaggedTrans SlotsRedirectTag IdentityT

runSlotsRedirect :: SlotsRedirect m a -> m a
runSlotsRedirect = coerce

askSlottingContextSum :: MonadSlottingSum m => m SlottingContextSum
askSlottingContextSum = Ether.ask'

type SlotsSumEnv m = (MonadSlottingSum m, NtpMode m, SimpleSlottingMode m)

getCurrentSlotSum :: SlotsSumEnv m => m (Maybe SlotId)
getCurrentSlotSum = helper getCurrentSlot

getCurrentSlotBlockingSum :: SlotsSumEnv m => m SlotId
getCurrentSlotBlockingSum = helper getCurrentSlotBlocking

getCurrentSlotInaccurateSum :: SlotsSumEnv m => m SlotId
getCurrentSlotInaccurateSum = helper getCurrentSlotInaccurate

currentTimeSlottingSum :: SlotsSumEnv m => m Timestamp
currentTimeSlottingSum = helper currentTimeSlotting

instance (SlotsSumEnv m, t ~ IdentityT) =>
         MonadSlots (Ether.TaggedTrans SlotsRedirectTag t m) where
    getCurrentSlot = getCurrentSlotSum
    getCurrentSlotBlocking = getCurrentSlotBlockingSum
    getCurrentSlotInaccurate = getCurrentSlotInaccurateSum
    currentTimeSlotting = currentTimeSlottingSum

helper
    :: SlotsSumEnv m
    => (forall n. MonadSlots n => n a)
    -> m a
helper action =
    Ether.ask' >>= \case
        SCSimple -> runSimpleSlotsRedirect action
        SCNtp var -> Ether.runReaderT' (runNtpSlotsRedirect action) var

type SlottingWorkerModeSum m = NtpWorkerMode m

-- | Get all slotting workers using 'SlottingContextSum'.
slottingWorkers :: SlottingWorkerModeSum m => SlottingContextSum -> [m ()]
slottingWorkers SCSimple    = []
slottingWorkers (SCNtp var) = ntpWorkers var
