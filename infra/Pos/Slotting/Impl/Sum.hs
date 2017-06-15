{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Sum of slotting implementations.

module Pos.Slotting.Impl.Sum
       ( SlottingContextSum (..)
       , MonadSlottingSum
       , SlotsRedirect
       , runSlotsRedirect
       , askSlottingContextSum
       , getCurrentSlotReal
       , getCurrentSlotBlockingReal
       , getCurrentSlotInaccurateReal
       , currentTimeSlottingReal

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

type SlottingModeSum m = (NtpMode m, SimpleSlottingMode m)

type SlotsRealMonad m =
    (MonadSlottingSum m, SlottingModeSum m)


getCurrentSlotReal :: SlotsRealMonad m => m (Maybe SlotId)
getCurrentSlotReal = helper getCurrentSlot

getCurrentSlotBlockingReal :: SlotsRealMonad m => m SlotId
getCurrentSlotBlockingReal = helper getCurrentSlotBlocking

getCurrentSlotInaccurateReal :: SlotsRealMonad m => m SlotId
getCurrentSlotInaccurateReal = helper getCurrentSlotInaccurate

currentTimeSlottingReal :: SlotsRealMonad m => m Timestamp
currentTimeSlottingReal = helper currentTimeSlotting

instance (MonadSlottingSum m, SlottingModeSum m, t ~ IdentityT) =>
         MonadSlots (Ether.TaggedTrans SlotsRedirectTag t m) where
    getCurrentSlot = getCurrentSlotReal
    getCurrentSlotBlocking = getCurrentSlotBlockingReal
    getCurrentSlotInaccurate = getCurrentSlotInaccurateReal
    currentTimeSlotting = currentTimeSlottingReal

helper
    :: (MonadSlottingSum m, SlottingModeSum m)
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
