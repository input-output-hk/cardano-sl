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

import           Mockable (CurrentTime, Mockable, currentTime)
import           NTP.Example ()

import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Core.Slotting (SlotId (..), Timestamp (..), unflattenSlotId)
import           Pos.Slotting.Impl.Util (approxSlotUsingOutdated, slotFromTimestamp)
import           Pos.Slotting.MemState (MonadSlotsData, getCurrentNextEpochIndexM,
                                        waitCurrentEpochEqualsM)

----------------------------------------------------------------------------
-- Mode
----------------------------------------------------------------------------

type SimpleSlottingMode ctx m
    = ( Mockable CurrentTime m
      , MonadSlotsData ctx m
      , MonadIO m
      , HasConfiguration)

----------------------------------------------------------------------------
-- State
----------------------------------------------------------------------------

data SimpleSlottingState = SimpleSlottingState
    { _sssLastSlot :: !SlotId
    }

type SimpleSlottingVar = TVar SimpleSlottingState

mkSimpleSlottingVar :: (MonadIO m, HasConfiguration) => m SimpleSlottingVar
mkSimpleSlottingVar = atomically $ newTVar $ SimpleSlottingState $ unflattenSlotId 0

----------------------------------------------------------------------------
-- Implementation
----------------------------------------------------------------------------

getCurrentSlotSimple
    :: (SimpleSlottingMode ctx m)
    => SimpleSlottingVar
    -> m (Maybe SlotId)
getCurrentSlotSimple var = traverse (updateLastSlot var) =<< (currentTimeSlottingSimple >>= slotFromTimestamp)

getCurrentSlotBlockingSimple
    :: (SimpleSlottingMode ctx m)
    => SimpleSlottingVar
    -> m SlotId
getCurrentSlotBlockingSimple var = do
    (_, nextEpochIndex) <- getCurrentNextEpochIndexM
    getCurrentSlotSimple var >>= \case
        Just slot -> pure slot
        Nothing -> do
            waitCurrentEpochEqualsM nextEpochIndex
            getCurrentSlotBlockingSimple var

getCurrentSlotInaccurateSimple
    :: (SimpleSlottingMode ctx m)
    => SimpleSlottingVar
    -> m SlotId
getCurrentSlotInaccurateSimple var =
    getCurrentSlotSimple var >>= \case
        Just slot -> pure slot
        Nothing   -> do
            lastSlot <- _sssLastSlot <$> atomically (readTVar var)
            max lastSlot <$> (currentTimeSlottingSimple >>=
                approxSlotUsingOutdated)

currentTimeSlottingSimple :: (SimpleSlottingMode ctx m) => m Timestamp
currentTimeSlottingSimple = Timestamp <$> currentTime

updateLastSlot :: MonadIO m => SimpleSlottingVar -> SlotId -> m SlotId
updateLastSlot var slot = atomically $ do
    modifyTVar' var (SimpleSlottingState . max slot . _sssLastSlot)
    _sssLastSlot <$> readTVar var
