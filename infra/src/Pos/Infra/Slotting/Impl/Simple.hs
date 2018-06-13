{-# LANGUAGE TypeFamilies #-}

-- | Simple implementation of slotting.

module Pos.Infra.Slotting.Impl.Simple
       ( SimpleSlottingStateVar
       , mkSimpleSlottingStateVar

       , SimpleSlottingMode
       , MonadSimpleSlotting
       , getCurrentSlotSimple
       , getCurrentSlotSimple'
       , getCurrentSlotBlockingSimple
       , getCurrentSlotBlockingSimple'
       , getCurrentSlotInaccurateSimple
       , getCurrentSlotInaccurateSimple'
       , currentTimeSlottingSimple
       ) where

import           Universum

import           Mockable (CurrentTime, Mockable, currentTime)

import           Pos.Core.Slotting (SlotCount, SlotId (..), Timestamp (..),
                     unflattenSlotId)
import           Pos.Infra.Slotting.Impl.Util (approxSlotUsingOutdated,
                     slotFromTimestamp)
import           Pos.Infra.Slotting.MemState (MonadSlotsData,
                     getCurrentNextEpochIndexM, waitCurrentEpochEqualsM)
import           Pos.Util (HasLens (..))

----------------------------------------------------------------------------
-- Mode
----------------------------------------------------------------------------

type SimpleSlottingMode ctx m
    = ( Mockable CurrentTime m
      , MonadSlotsData ctx m
      , MonadIO m
      )

type MonadSimpleSlotting ctx m
    = ( MonadReader ctx m
      , HasLens SimpleSlottingStateVar ctx SimpleSlottingStateVar
      , SimpleSlottingMode ctx m
      )

----------------------------------------------------------------------------
-- State
----------------------------------------------------------------------------

data SimpleSlottingState = SimpleSlottingState
    { _sssLastSlot :: !SlotId
    }

type SimpleSlottingStateVar = TVar SimpleSlottingState

mkSimpleSlottingStateVar :: MonadIO m => SlotCount -> m SimpleSlottingStateVar
mkSimpleSlottingStateVar epochSlots =
    atomically $ newTVar $ SimpleSlottingState $ unflattenSlotId epochSlots 0

----------------------------------------------------------------------------
-- Implementation
----------------------------------------------------------------------------

getCurrentSlotSimple'
    :: SimpleSlottingMode ctx m
    => SlotCount
    -> SimpleSlottingStateVar
    -> m (Maybe SlotId)
getCurrentSlotSimple' epochSlots var =
        currentTimeSlottingSimple
    >>= slotFromTimestamp epochSlots
    >>= traverse (updateLastSlot var)

getCurrentSlotSimple
    :: MonadSimpleSlotting ctx m => SlotCount -> m (Maybe SlotId)
getCurrentSlotSimple epochSlots =
    view (lensOf @SimpleSlottingStateVar) >>= getCurrentSlotSimple' epochSlots

getCurrentSlotBlockingSimple'
    :: SimpleSlottingMode ctx m
    => SlotCount
    -> SimpleSlottingStateVar
    -> m SlotId
getCurrentSlotBlockingSimple' epochSlots var = do
    (_, nextEpochIndex) <- getCurrentNextEpochIndexM
    getCurrentSlotSimple' epochSlots var >>= \case
        Just slot -> pure slot
        Nothing   -> do
            waitCurrentEpochEqualsM nextEpochIndex
            getCurrentSlotBlockingSimple' epochSlots var

getCurrentSlotBlockingSimple
    :: MonadSimpleSlotting ctx m => SlotCount -> m SlotId
getCurrentSlotBlockingSimple epochSlots = view (lensOf @SimpleSlottingStateVar)
    >>= getCurrentSlotBlockingSimple' epochSlots

getCurrentSlotInaccurateSimple'
    :: SimpleSlottingMode ctx m
    => SlotCount
    -> SimpleSlottingStateVar
    -> m SlotId
getCurrentSlotInaccurateSimple' epochSlots var =
    getCurrentSlotSimple' epochSlots var >>= \case
        Just slot -> pure slot
        Nothing   -> do
            lastSlot <- _sssLastSlot <$> atomically (readTVar var)
            max lastSlot <$> (currentTimeSlottingSimple >>=
                approxSlotUsingOutdated epochSlots)

getCurrentSlotInaccurateSimple
    :: MonadSimpleSlotting ctx m => SlotCount -> m SlotId
getCurrentSlotInaccurateSimple epochSlots =
    view (lensOf @SimpleSlottingStateVar)
        >>= getCurrentSlotInaccurateSimple' epochSlots

currentTimeSlottingSimple :: SimpleSlottingMode ctx m => m Timestamp
currentTimeSlottingSimple = Timestamp <$> currentTime

updateLastSlot :: MonadIO m => SimpleSlottingStateVar -> SlotId -> m SlotId
updateLastSlot var slot = atomically $ do
    modifyTVar' var (SimpleSlottingState . max slot . _sssLastSlot)
    _sssLastSlot <$> readTVar var
