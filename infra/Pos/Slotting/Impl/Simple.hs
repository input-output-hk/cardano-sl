{-# LANGUAGE TypeFamilies #-}

-- | Simple implementation of slotting.

module Pos.Slotting.Impl.Simple
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

import           Pos.Core.Configuration (HasProtocolConstants)
import           Pos.Core.Slotting (SlotId (..), Timestamp (..), unflattenSlotId)
import           Pos.Slotting.Impl.Util (approxSlotUsingOutdated, slotFromTimestamp)
import           Pos.Slotting.MemState (MonadSlotsData, getCurrentNextEpochIndexM,
                                        waitCurrentEpochEqualsM)
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

mkSimpleSlottingStateVar :: (MonadIO m, HasProtocolConstants) => m SimpleSlottingStateVar
mkSimpleSlottingStateVar = atomically $ newTVar $ SimpleSlottingState $ unflattenSlotId 0

----------------------------------------------------------------------------
-- Implementation
----------------------------------------------------------------------------

getCurrentSlotSimple'
    :: (SimpleSlottingMode ctx m, HasProtocolConstants)
    => SimpleSlottingStateVar
    -> m (Maybe SlotId)
getCurrentSlotSimple' var =
        currentTimeSlottingSimple
    >>= slotFromTimestamp
    >>= traverse (updateLastSlot var)

getCurrentSlotSimple
    :: (MonadSimpleSlotting ctx m, HasProtocolConstants)
    => m (Maybe SlotId)
getCurrentSlotSimple = view (lensOf @SimpleSlottingStateVar) >>= getCurrentSlotSimple'

getCurrentSlotBlockingSimple'
    :: (SimpleSlottingMode ctx m, HasProtocolConstants)
    => SimpleSlottingStateVar
    -> m SlotId
getCurrentSlotBlockingSimple' var = do
    (_, nextEpochIndex) <- getCurrentNextEpochIndexM
    getCurrentSlotSimple' var >>= \case
        Just slot -> pure slot
        Nothing -> do
            waitCurrentEpochEqualsM nextEpochIndex
            getCurrentSlotBlockingSimple' var

getCurrentSlotBlockingSimple
    :: (MonadSimpleSlotting ctx m, HasProtocolConstants)
    => m SlotId
getCurrentSlotBlockingSimple =
    view (lensOf @SimpleSlottingStateVar) >>= getCurrentSlotBlockingSimple'

getCurrentSlotInaccurateSimple'
    :: (SimpleSlottingMode ctx m, HasProtocolConstants)
    => SimpleSlottingStateVar
    -> m SlotId
getCurrentSlotInaccurateSimple' var =
    getCurrentSlotSimple' var >>= \case
        Just slot -> pure slot
        Nothing   -> do
            lastSlot <- _sssLastSlot <$> atomically (readTVar var)
            max lastSlot <$> (currentTimeSlottingSimple >>=
                approxSlotUsingOutdated)

getCurrentSlotInaccurateSimple
    :: (MonadSimpleSlotting ctx m, HasProtocolConstants)
    => m SlotId
getCurrentSlotInaccurateSimple =
    view (lensOf @SimpleSlottingStateVar) >>= getCurrentSlotInaccurateSimple'

currentTimeSlottingSimple :: (SimpleSlottingMode ctx m) => m Timestamp
currentTimeSlottingSimple = Timestamp <$> currentTime

updateLastSlot :: MonadIO m => SimpleSlottingStateVar -> SlotId -> m SlotId
updateLastSlot var slot = atomically $ do
    modifyTVar' var (SimpleSlottingState . max slot . _sssLastSlot)
    _sssLastSlot <$> readTVar var
