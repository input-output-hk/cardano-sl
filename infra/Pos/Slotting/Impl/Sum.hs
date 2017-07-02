{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Sum of slotting implementations.

module Pos.Slotting.Impl.Sum
       ( SlottingContextSum (..)
       , MonadSlottingSum
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

import           Ether.Internal           (HasLens (..))

import           Pos.Core.Types           (SlotId (..), Timestamp)
import           Pos.Slotting.Impl.Ntp    (NtpMode, NtpSlottingVar, NtpWorkerMode,
                                           ntpCurrentTime, ntpGetCurrentSlot,
                                           ntpGetCurrentSlotBlocking,
                                           ntpGetCurrentSlotInaccurate, ntpWorkers)
import           Pos.Slotting.Impl.Simple (SimpleSlottingMode, currentTimeSlottingSimple,
                                           getCurrentSlotBlockingSimple,
                                           getCurrentSlotInaccurateSimple,
                                           getCurrentSlotSimple)

-- | Sum of all contexts used by slotting implementations.
data SlottingContextSum
    = SCSimple
    | SCNtp NtpSlottingVar

-- | Monad which combines all 'MonadSlots' implementations (and
-- uses only one of them).
type MonadSlottingSum ctx m = (MonadReader ctx m, HasLens SlottingContextSum ctx SlottingContextSum)

askSlottingContextSum :: MonadSlottingSum ctx m => m SlottingContextSum
askSlottingContextSum = view (lensOf @SlottingContextSum)

type SlotsSumEnv ctx m = (MonadSlottingSum ctx m, NtpMode m, SimpleSlottingMode m)

getCurrentSlotSum :: SlotsSumEnv ctx m => m (Maybe SlotId)
getCurrentSlotSum =
    view (lensOf @SlottingContextSum) >>= \case
        SCSimple -> getCurrentSlotSimple
        SCNtp var -> ntpGetCurrentSlot var

getCurrentSlotBlockingSum :: SlotsSumEnv ctx m => m SlotId
getCurrentSlotBlockingSum =
    view (lensOf @SlottingContextSum) >>= \case
        SCSimple -> getCurrentSlotBlockingSimple
        SCNtp var -> ntpGetCurrentSlotBlocking var

getCurrentSlotInaccurateSum :: SlotsSumEnv ctx m => m SlotId
getCurrentSlotInaccurateSum =
    view (lensOf @SlottingContextSum) >>= \case
        SCSimple -> getCurrentSlotInaccurateSimple
        SCNtp var -> ntpGetCurrentSlotInaccurate var

currentTimeSlottingSum :: SlotsSumEnv ctx m => m Timestamp
currentTimeSlottingSum =
    view (lensOf @SlottingContextSum) >>= \case
        SCSimple -> currentTimeSlottingSimple
        SCNtp var -> ntpCurrentTime var

type SlottingWorkerModeSum m = NtpWorkerMode m

-- | Get all slotting workers using 'SlottingContextSum'.
slottingWorkers :: SlottingWorkerModeSum m => SlottingContextSum -> [m ()]
slottingWorkers SCSimple    = []
slottingWorkers (SCNtp var) = ntpWorkers var
