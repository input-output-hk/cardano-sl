{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}

-- | Slotting utilities.

module Pos.Core.Slotting.Util
       (
         -- * Helpers using 'MonadSlots[Data]'
         getCurrentSlotFlat
       , slotFromTimestamp

         -- * Worker which ticks when slot starts and its parameters
       , OnNewSlotParams (..)
       , defaultOnNewSlotParams
       , ActionTerminationPolicy (..)
       ) where

import           Universum

import           Data.Time.Units (Microsecond, convertUnit)

import           Pos.Core.Configuration.Protocol (HasProtocolConstants,
                     epochSlots)
import           Pos.Core.Slotting.Class (MonadSlots (..), MonadSlotsData)
import           Pos.Core.Slotting.EpochIndex (EpochIndex (..))
import           Pos.Core.Slotting.LocalSlotIndex (LocalSlotIndex (..),
                     mkLocalSlotIndex)
import           Pos.Core.Slotting.MemState (getSystemStartM,
                     withSlottingVarAtomM)
import           Pos.Core.Slotting.SlotId (FlatSlotId, SlotId (..),
                     flattenSlotId)
import           Pos.Core.Slotting.TimeDiff (addTimeDiffToTimestamp)
import           Pos.Core.Slotting.Timestamp (Timestamp (..))
import           Pos.Core.Slotting.Types (EpochSlottingData (..), SlottingData,
                     getAllEpochIndices, lookupEpochSlottingData)

import           Pos.Util.Util (leftToPanic)



-- | Get flat id of current slot based on MonadSlots.
getCurrentSlotFlat :: (MonadSlots ctx m, HasProtocolConstants) => m (Maybe FlatSlotId)
getCurrentSlotFlat = fmap flattenSlotId <$> getCurrentSlot


-- | Parameters for `onNewSlot`.
data OnNewSlotParams = OnNewSlotParams
    { onspStartImmediately  :: !Bool
    -- ^ Whether first action should be executed ASAP (i. e. basically
    -- when the program starts), or only when new slot starts.
    --
    -- For example, if the program is started in the middle of a slot
    -- and this parameter in 'False', we will wait for half of slot
    -- and only then will do something.
    , onspTerminationPolicy :: !ActionTerminationPolicy
    -- ^ What should be done if given action doesn't finish before new
    -- slot starts. See the description of 'ActionTerminationPolicy'.
    }

-- | Default parameters which were used by almost all code before this
-- data type was introduced.
defaultOnNewSlotParams :: OnNewSlotParams
defaultOnNewSlotParams =
    OnNewSlotParams
    { onspStartImmediately = True
    , onspTerminationPolicy = NoTerminationPolicy
    }

-- | This policy specifies what should be done if the action passed to
-- `onNewSlot` doesn't finish when current slot finishes.
--
-- We don't want to run given action more than once in parallel for
-- variety of reasons:
-- 1. If action hangs for some reason, there can be infinitely growing pool
-- of hanging actions with probably bad consequences (e. g. leaking memory).
-- 2. Thread management will be quite complicated if we want to fork
-- threads inside `onNewSlot`.
-- 3. If more than one action is launched, they may use same resources
-- concurrently, so the code must account for it.
data ActionTerminationPolicy
    = NoTerminationPolicy
    -- ^ Even if action keeps running after current slot finishes,
    -- we'll just wait and start action again only after the previous
    -- one finishes.
    | NewSlotTerminationPolicy !Text
    -- ^ If new slot starts, running action will be cancelled. Name of
    -- the action should be passed for logging.

-- | Compute current slot from current timestamp based on data
-- provided by 'MonadSlotsData'.
slotFromTimestamp
    :: (MonadSlotsData ctx m, HasProtocolConstants)
    => Timestamp
    -> m (Maybe SlotId)
slotFromTimestamp approxCurTime = do
    systemStart <- getSystemStartM
    withSlottingVarAtomM (iterateBackwardsSearch systemStart)
  where
    iterateBackwardsSearch
        :: Timestamp
        -> SlottingData
        -> Maybe SlotId
    iterateBackwardsSearch systemStart slottingData = do
        let allEpochIndex = getAllEpochIndices slottingData

        -- We first reverse the indices since the most common calls to this funcion
        -- will be from next/current index and close.
        let reversedEpochIndices = reverse allEpochIndex

        let iterateIndicesUntilJust
                :: [EpochIndex]
                -> Maybe SlotId
            iterateIndicesUntilJust []      = Nothing
            iterateIndicesUntilJust indices = do

              -- Get current index or return @Nothing@ if the indices list is empty.
              let mCurrentEpochIndex :: Maybe EpochIndex
                  mCurrentEpochIndex = case indices of
                      (x:_) -> Just x
                      _     -> Nothing
              -- Try to find a slot.
              let mFoundSlot = mCurrentEpochIndex >>= findSlot systemStart slottingData

              case mFoundSlot of
                -- If you found a slot, then return it
                Just foundSlot -> Just foundSlot
                -- If no slot is found, iterate with the rest of the list
                Nothing        -> iterateIndicesUntilJust $ case indices of
                    []     -> []
                    (_:xs) -> xs


        -- Iterate the indices recursively with the reverse indices, starting
        -- with the most recent one.
        iterateIndicesUntilJust reversedEpochIndices


    -- Find a slot using timestamps. If no @EpochSlottingData@ is found return
    -- @Nothing@.
    findSlot
        :: Timestamp
        -> SlottingData
        -> EpochIndex
        -> Maybe SlotId
    findSlot systemStart slottingData epochIndex = do
        epochSlottingData <- lookupEpochSlottingData epochIndex slottingData
        computeSlotUsingEpoch systemStart approxCurTime epochIndex epochSlottingData

    computeSlotUsingEpoch
        :: Timestamp
        -> Timestamp
        -> EpochIndex
        -> EpochSlottingData
        -> Maybe SlotId
    computeSlotUsingEpoch systemStart (Timestamp curTime) epoch EpochSlottingData {..}
        | curTime < epochStart = Nothing
        | curTime < epochStart + epochDuration = Just $ SlotId epoch localSlot
        | otherwise = Nothing
      where
        epochStart :: Microsecond
        epochStart = getTimestamp (esdStartDiff `addTimeDiffToTimestamp` systemStart)

        localSlotNumeric :: Word16
        localSlotNumeric = fromIntegral $ (curTime - epochStart) `div` slotDuration

        localSlot :: LocalSlotIndex
        localSlot =
            leftToPanic "computeSlotUsingEpoch: " $
            mkLocalSlotIndex localSlotNumeric

        slotDuration :: Microsecond
        slotDuration = convertUnit esdSlotDuration

        epochDuration :: Microsecond
        epochDuration = slotDuration * fromIntegral epochSlots
