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

import qualified Ether

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
type MonadSlottingSum = Ether.MonadReader' SlottingContextSum

askSlottingContextSum :: MonadSlottingSum m => m SlottingContextSum
askSlottingContextSum = Ether.ask'

type SlotsSumEnv m = (MonadSlottingSum m, NtpMode m, SimpleSlottingMode m)

getCurrentSlotSum :: SlotsSumEnv m => m (Maybe SlotId)
getCurrentSlotSum =
    Ether.ask' >>= \case
        SCSimple -> simpleGetCurrentSlot
        SCNtp var -> ntpGetCurrentSlot var

getCurrentSlotBlockingSum :: SlotsSumEnv m => m SlotId
getCurrentSlotBlockingSum =
    Ether.ask' >>= \case
        SCSimple -> simpleGetCurrentSlotBlocking
        SCNtp var -> ntpGetCurrentSlotBlocking var

getCurrentSlotInaccurateSum :: SlotsSumEnv m => m SlotId
getCurrentSlotInaccurateSum =
    Ether.ask' >>= \case
        SCSimple -> simpleGetCurrentSlotInaccurate
        SCNtp var -> ntpGetCurrentSlotInaccurate var

currentTimeSlottingSum :: SlotsSumEnv m => m Timestamp
currentTimeSlottingSum =
    Ether.ask' >>= \case
        SCSimple -> simpleCurrentTimeSlotting
        SCNtp var -> ntpCurrentTime var

type SlottingWorkerModeSum m = NtpWorkerMode m

-- | Get all slotting workers using 'SlottingContextSum'.
slottingWorkers :: SlottingWorkerModeSum m => SlottingContextSum -> [m ()]
slottingWorkers SCSimple    = []
slottingWorkers (SCNtp var) = ntpWorkers var
