-- | Core types used in 'Slotting'.

module Pos.Slotting.Types
       ( EpochSlottingData (..)
       , SlottingData
       , getSlottingDataMap
       , createSlottingDataUnsafe
       , createInitSlottingData
       , getLastEpochIndex
       , getPenultEpochIndex
       , getLastEpochSlottingData
       , getPenultEpoch
       , addEpochSlottingData
       , lookupEpochSlottingData
       , computeSlotStart
       ) where

import           Universum

import           Data.Map.Strict as M
import           Data.Time.Units (Millisecond, convertUnit)

import           Pos.Core        (EpochIndex, EpochIndex (..),
                                  LocalSlotIndex (..), SlotId (..),
                                  TimeDiff (..), Timestamp (..),
                                  addTimeDiffToTimestamp, getSlotIndex)
import           Pos.Util.Util   ()


-- | Data which is necessary for slotting and corresponds to a particular epoch.
data EpochSlottingData = EpochSlottingData
    { esdSlotDuration :: !Millisecond
    -- ^ Slot duration actual for given epoch.
    , esdStartDiff    :: !TimeDiff
    -- ^ Difference between epoch start and system start time
    } deriving (Eq, Show, Generic)

instance NFData EpochSlottingData


-- | Data necessary for slotting to work which is basically part of GState.
-- External code can use functions like getLastEpochIndex or getLastEpochSlottingData and 
-- not worry about cases where it doesn't exist (this module should be responsible for it).
-- Note that it's important to use error rather than default values like 0, because 
-- such cases indicate invariants violation and shouldn't be hidden behind default values.
type PenultSlottingData = EpochSlottingData

newtype SlottingData = SlottingData 
    { getSlottingDataMap :: Map EpochIndex EpochSlottingData 
    } deriving (Eq, Show, Generic, Monoid)

instance NFData SlottingData

-- | Unsafe constructor that can lead to invalid state!
createSlottingDataUnsafe :: Map EpochIndex EpochSlottingData -> SlottingData
createSlottingDataUnsafe = SlottingData

-- | Restricted constructor function for the (initial) creation of @SlottingData@.
-- Optional: Wrap @EpochSlottingData@ into @PenultSlottingData@ using newtype.
createInitSlottingData 
    :: PenultSlottingData 
    -> EpochSlottingData 
    -> SlottingData
createInitSlottingData psd esd = SlottingData validInitialSlottingData
  where

    validInitialSlottingData :: Map EpochIndex EpochSlottingData
    validInitialSlottingData = M.union penultEpochSlottingData lastEpochSlottingData

    penultEpochSlottingData :: Map EpochIndex PenultSlottingData
    penultEpochSlottingData = M.singleton 0 psd

    lastEpochSlottingData :: Map EpochIndex EpochSlottingData
    lastEpochSlottingData = M.singleton 1 esd


-- | Get the latest epoch index.
getLastEpochIndex :: SlottingData -> EpochIndex
getLastEpochIndex = fst . M.findMax . getSlottingDataMap

-- | Get the latest epoch slotting data.
getLastEpochSlottingData :: SlottingData -> EpochSlottingData
getLastEpochSlottingData = snd . M.findMax . getSlottingDataMap

-- | Get the penultimate epoch index. Last epoch - 1.
getPenultEpochIndex :: SlottingData -> EpochIndex
getPenultEpochIndex = decreaseEpochIndex . getLastEpochIndex
  where
    decreaseEpochIndex :: EpochIndex -> EpochIndex
    decreaseEpochIndex ei = EpochIndex $ getEpochIndex ei - 1

-- | Get the penultimate epoch slotting data. Last epoch - 1.
getPenultEpoch :: SlottingData -> EpochSlottingData
getPenultEpoch sdp@(getSlottingDataMap -> sd) = sd M.! penultEpochIndex 
  where
    penultEpochIndex = getPenultEpochIndex sdp

-- | Lookup the slotting data for an arbitrary `EpochIndex`.
lookupEpochSlottingData :: EpochIndex -> SlottingData -> Maybe EpochSlottingData
lookupEpochSlottingData epochIndex slottingData = M.lookup epochIndex slottingData'
  where
    slottingData' :: Map EpochIndex EpochSlottingData
    slottingData' = getSlottingDataMap slottingData

-- | Insert `EpochSlottingData`.
addEpochSlottingData :: EpochIndex -> EpochSlottingData -> SlottingData -> SlottingData
addEpochSlottingData epochIndex epochSlottingData slottingData = 
    SlottingData $ M.insert epochIndex epochSlottingData slottingData'
  where
    slottingData' :: Map EpochIndex EpochSlottingData
    slottingData' = getSlottingDataMap slottingData

-- | Compute when the slot started.
computeSlotStart :: SlotId -> EpochSlottingData -> Timestamp
computeSlotStart slotId esd = slotTimestamp localSlotIndex esd
    where
      localSlotIndex :: LocalSlotIndex
      localSlotIndex = siSlot slotId

      slotTimestamp 
          :: LocalSlotIndex 
          -> EpochSlottingData 
          -> Timestamp
      slotTimestamp localSlotIndex' EpochSlottingData{..} =
          addTimeDiffToTimestamp esdStartDiff currentSlotTimestamp 
        where
          intSlotIndex :: Word16
          intSlotIndex = getSlotIndex localSlotIndex'

          currentSlotTimestamp :: Timestamp
          currentSlotTimestamp = 
              Timestamp (fromIntegral intSlotIndex * convertUnit esdSlotDuration)
