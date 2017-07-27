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

import           Pos.Core.Slotting           (unflattenSlotId)
import           Pos.Core.Types              (SlotId (..), Timestamp (..))
import           Pos.Slotting.Impl.Util      (approxSlotUsingOutdated, slotFromTimestamp)
import           Pos.Slotting.MemState.Class (MonadSlotsData (..))

----------------------------------------------------------------------------
-- Mode
----------------------------------------------------------------------------

type SimpleSlottingMode m =
    ( Mockable CurrentTime m
    , MonadSlotsData m
    , MonadIO m
    , MonadThrow m
    )

----------------------------------------------------------------------------
-- State
----------------------------------------------------------------------------

data SimpleSlottingState = SimpleSlottingState
    { _sssLastSlot :: !SlotId
    }

type SimpleSlottingVar = TVar SimpleSlottingState

mkSimpleSlottingVar :: MonadIO m => m SimpleSlottingVar
mkSimpleSlottingVar = atomically $ newTVar $ SimpleSlottingState $ unflattenSlotId 0

----------------------------------------------------------------------------
-- Implementation
----------------------------------------------------------------------------

getCurrentSlotSimple
    :: (SimpleSlottingMode m)
    => SimpleSlottingVar
    -> m (Maybe SlotId)
getCurrentSlotSimple var = traverse (updateLastSlot var) =<< (currentTimeSlottingSimple >>= slotFromTimestamp)

getCurrentSlotBlockingSimple
    :: (SimpleSlottingMode m)
    => SimpleSlottingVar
    -> m SlotId
getCurrentSlotBlockingSimple var = do
    currentEpochIndex <- getCurrentEpochIndexM
    getCurrentSlotSimple var >>= \case
        Just slot -> pure slot
        Nothing -> do
            waitCurrentEpochEqualsM (currentEpochIndex + 1)
            getCurrentSlotBlockingSimple var

getCurrentSlotInaccurateSimple
    :: (SimpleSlottingMode m)
    => SimpleSlottingVar
    -> m SlotId
getCurrentSlotInaccurateSimple var = do
    currentEpochIndex <- getCurrentEpochIndexM
    getCurrentSlotSimple var >>= \case
        Just slot -> pure slot
        Nothing   -> do
            lastSlot <- _sssLastSlot <$> atomically (readTVar var)
            max lastSlot <$> (currentTimeSlottingSimple >>=
                approxSlotUsingOutdated currentEpochIndex)

currentTimeSlottingSimple :: SimpleSlottingMode m => m Timestamp
currentTimeSlottingSimple = Timestamp <$> currentTime

updateLastSlot :: MonadIO m => SimpleSlottingVar -> SlotId -> m SlotId
updateLastSlot var slot = atomically $ do
    modifyTVar' var (SimpleSlottingState . max slot . _sssLastSlot)
    _sssLastSlot <$> readTVar var
