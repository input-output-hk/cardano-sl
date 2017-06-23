{-# LANGUAGE TypeFamilies #-}

-- | Simple implementation of slotting.

module Pos.Slotting.Impl.Simple
       ( SimpleSlottingMode
       , SimpleSlotsRedirect
       , runSimpleSlotsRedirect

       , getCurrentSlotSimple
       , getCurrentSlotBlockingSimple
       , getCurrentSlotInaccurateSimple
       , currentTimeSlottingSimple
       ) where

import           Universum

import           Control.Monad.Trans.Identity (IdentityT (..))
import           Data.Coerce                  (coerce)
import qualified Ether
import           Mockable                     (CurrentTime, Mockable, currentTime)
import           NTP.Example                  ()

import           Pos.Core.Types               (SlotId (..), Timestamp (..))
import           Pos.Slotting.Class           (MonadSlots (..))
import           Pos.Slotting.Impl.Util       (approxSlotUsingOutdated, slotFromTimestamp)
import           Pos.Slotting.MemState.Class  (MonadSlotsData (..))
import           Pos.Slotting.Types           (SlottingData (..))

----------------------------------------------------------------------------
-- Mode
----------------------------------------------------------------------------

type SimpleSlottingMode m = (Mockable CurrentTime m, MonadSlotsData m)

----------------------------------------------------------------------------
-- MonadSlots implementation
----------------------------------------------------------------------------

data SimpleSlotsRedirectTag

type SimpleSlotsRedirect =
    Ether.TaggedTrans SimpleSlotsRedirectTag IdentityT

runSimpleSlotsRedirect :: SimpleSlotsRedirect m a -> m a
runSimpleSlotsRedirect = coerce

instance (SimpleSlottingMode m, t ~ IdentityT) =>
         MonadSlots (Ether.TaggedTrans SimpleSlotsRedirectTag t m) where
    getCurrentSlot = getCurrentSlotSimple
    getCurrentSlotBlocking = getCurrentSlotBlockingSimple
    getCurrentSlotInaccurate = getCurrentSlotInaccurateSimple
    currentTimeSlotting = currentTimeSlottingSimple

----------------------------------------------------------------------------
-- Implementation
----------------------------------------------------------------------------

getCurrentSlotSimple :: SimpleSlottingMode m => m (Maybe SlotId)
getCurrentSlotSimple = currentTimeSlottingSimple >>= slotFromTimestamp

getCurrentSlotBlockingSimple :: SimpleSlottingMode m => m SlotId
getCurrentSlotBlockingSimple = do
    penult <- sdPenultEpoch <$> getSlottingData
    getCurrentSlotSimple >>= \case
        Just slot -> pure slot
        Nothing -> do
            waitPenultEpochEquals (penult + 1)
            getCurrentSlotBlockingSimple

getCurrentSlotInaccurateSimple :: SimpleSlottingMode m => m SlotId
getCurrentSlotInaccurateSimple = do
    penult <- sdPenultEpoch <$> getSlottingData
    getCurrentSlotSimple >>= \case
        Just slot -> pure slot
        Nothing -> currentTimeSlottingSimple >>= approxSlotUsingOutdated penult

currentTimeSlottingSimple :: SimpleSlottingMode m => m Timestamp
currentTimeSlottingSimple = Timestamp <$> currentTime
