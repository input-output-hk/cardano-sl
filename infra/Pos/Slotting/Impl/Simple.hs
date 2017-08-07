{-# LANGUAGE TypeFamilies #-}

-- | Simple implementation of slotting.

module Pos.Slotting.Impl.Simple
       ( SimpleSlottingVar
       , mkSimpleSlottingVar

       , SimpleSlottingMode
       , getCurrentSlotSimple
       , getCurrentSlotBlockingSimple
       , getCurrentSlotInaccurateSimple
       , currentTimeSlottingSimple
       ) where

import           Universum

import           Mockable                    (CurrentTime, Mockable, currentTime)
import           NTP.Example                 ()

import           Pos.Core                    (HasCoreConstants, SlotId (..),
                                              Timestamp (..))
import           Pos.Slotting.Impl.Util      (approxSlotUsingOutdated, slotFromTimestamp)
import           Pos.Slotting.MemState.Class (MonadSlotsData (..))
import           Pos.Slotting.Types          (SlottingData (..))

----------------------------------------------------------------------------
-- Mode
----------------------------------------------------------------------------

type SimpleSlottingMode ctx m
     = ( Mockable CurrentTime m
       , MonadSlotsData m
       , MonadIO m
       , MonadReader ctx m
       , HasCoreConstants ctx)

----------------------------------------------------------------------------
-- State
----------------------------------------------------------------------------

data SimpleSlottingState = SimpleSlottingState
    { _sssLastSlot :: !SlotId
    }

type SimpleSlottingVar = TVar SimpleSlottingState

mkSimpleSlottingVar :: MonadIO m => m SimpleSlottingVar
mkSimpleSlottingVar = atomically $ newTVar $ SimpleSlottingState $ SlotId 0 0

----------------------------------------------------------------------------
-- Implementation
----------------------------------------------------------------------------

getCurrentSlotSimple ::
       SimpleSlottingMode ctx m => SimpleSlottingVar -> m (Maybe SlotId)
getCurrentSlotSimple var = traverse (updateLastSlot var) =<< (currentTimeSlottingSimple >>= slotFromTimestamp)

getCurrentSlotBlockingSimple ::
       SimpleSlottingMode ctx m => SimpleSlottingVar -> m SlotId
getCurrentSlotBlockingSimple var = do
    penult <- sdPenultEpoch <$> getSlottingData
    getCurrentSlotSimple var >>= \case
        Just slot -> pure slot
        Nothing -> do
            waitPenultEpochEquals (penult + 1)
            getCurrentSlotBlockingSimple var

getCurrentSlotInaccurateSimple ::
       SimpleSlottingMode ctx m => SimpleSlottingVar -> m SlotId
getCurrentSlotInaccurateSimple var = do
    penult <- sdPenultEpoch <$> getSlottingData
    getCurrentSlotSimple var >>= \case
        Just slot -> pure slot
        Nothing   -> do
            lastSlot <- _sssLastSlot <$> atomically (readTVar var)
            max lastSlot <$> (currentTimeSlottingSimple >>= approxSlotUsingOutdated penult)

currentTimeSlottingSimple :: SimpleSlottingMode ctx m => m Timestamp
currentTimeSlottingSimple = Timestamp <$> currentTime

updateLastSlot :: MonadIO m => SimpleSlottingVar -> SlotId -> m SlotId
updateLastSlot var slot = atomically $ do
    modifyTVar' var (SimpleSlottingState . max slot . _sssLastSlot)
    _sssLastSlot <$> readTVar var
