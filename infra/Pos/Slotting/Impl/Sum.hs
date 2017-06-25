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

import           EtherCompat

import           Pos.Core.Types           (SlotId (..), Timestamp)
import           Pos.Slotting.Impl.Ntp    (NtpMode, NtpSlottingVar, NtpWorkerMode,
                                           ntpCurrentTime, ntpGetCurrentSlot,
                                           ntpGetCurrentSlotBlocking,
                                           ntpGetCurrentSlotInaccurate, ntpWorkers)
import           Pos.Slotting.Impl.Simple (SimpleSlottingMode, simpleCurrentTimeSlotting,
                                           simpleGetCurrentSlot,
                                           simpleGetCurrentSlotBlocking,
                                           simpleGetCurrentSlotInaccurate)

-- | Sum of all contexts used by slotting implementations.
data SlottingContextSum
    = SCSimple
    | SCNtp NtpSlottingVar

-- | Monad which combines all 'MonadSlots' implementations (and
-- uses only one of them).
type MonadSlottingSum ctx m = MonadCtx ctx SlottingContextSum SlottingContextSum m

askSlottingContextSum :: MonadSlottingSum ctx m => m SlottingContextSum
askSlottingContextSum = askCtx @SlottingContextSum

type SlotsSumEnv ctx m = (MonadSlottingSum ctx m, NtpMode m, SimpleSlottingMode m)

getCurrentSlotSum :: SlotsSumEnv ctx m => m (Maybe SlotId)
getCurrentSlotSum =
    askCtx @SlottingContextSum >>= \case
        SCSimple -> simpleGetCurrentSlot
        SCNtp var -> ntpGetCurrentSlot var

getCurrentSlotBlockingSum :: SlotsSumEnv ctx m => m SlotId
getCurrentSlotBlockingSum =
    askCtx @SlottingContextSum >>= \case
        SCSimple -> simpleGetCurrentSlotBlocking
        SCNtp var -> ntpGetCurrentSlotBlocking var

getCurrentSlotInaccurateSum :: SlotsSumEnv ctx m => m SlotId
getCurrentSlotInaccurateSum =
    askCtx @SlottingContextSum >>= \case
        SCSimple -> simpleGetCurrentSlotInaccurate
        SCNtp var -> ntpGetCurrentSlotInaccurate var

currentTimeSlottingSum :: SlotsSumEnv ctx m => m Timestamp
currentTimeSlottingSum =
    askCtx @SlottingContextSum >>= \case
        SCSimple -> simpleCurrentTimeSlotting
        SCNtp var -> ntpCurrentTime var

type SlottingWorkerModeSum m = NtpWorkerMode m

-- | Get all slotting workers using 'SlottingContextSum'.
slottingWorkers :: SlottingWorkerModeSum m => SlottingContextSum -> [m ()]
slottingWorkers SCSimple    = []
slottingWorkers (SCNtp var) = ntpWorkers var
