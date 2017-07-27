-- | Core types used in 'Slotting'.

module Pos.Slotting.Types
       ( EpochSlottingData (..)
       , SlottingData
       , getSlottingDataMap
       , createSlottingDataUnsafe
       , createInitSlottingData
       , getAllEpochIndex
       , getCurrentEpochIndex
       , getCurrentEpochSlottingData
       , getNextEpochIndex
       , getNextEpochSlottingData
       , addEpochSlottingData
       , lookupEpochSlottingData
       , computeSlotStart
       ) where

import           Universum

import           Data.Map.Strict as M
import           Data.Time.Units (Millisecond, convertUnit)

import           Pos.Core        (EpochIndex, EpochIndex (..), LocalSlotIndex (..),
                                  SlotId (..), TimeDiff (..), Timestamp (..),
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

-- Helpful type aliases
type CurrentEpochSlottingData = EpochSlottingData
type NextEpochSlottingData    = EpochSlottingData

-- | Data necessary for slotting to work which is basically part of GState.
-- Note that it's important to use error rather than default values like 0, because
-- such cases indicate invariants violation and shouldn't be hidden behind default values.
newtype SlottingData = SlottingData
    { getSlottingDataMap :: Map EpochIndex EpochSlottingData
    -- ^ Map containing the @EpochSlottingData@ for all the (known) @Epoch@
    } deriving (Eq, Show, Generic, Monoid)

instance NFData SlottingData

-- | Unsafe constructor that can lead to unsafe crash!
createSlottingDataUnsafe :: Map EpochIndex EpochSlottingData -> SlottingData
createSlottingDataUnsafe epochSlottingDataMap =
    if M.size epochSlottingDataMap < 2
        then criticalError
        else SlottingData epochSlottingDataMap
  where
    criticalError = error "It's impossible to create slotting data without two epochs."

-- | Restricted constructor function for the (initial) creation of @SlottingData@.
createInitSlottingData
    :: CurrentEpochSlottingData
    -> NextEpochSlottingData
    -> SlottingData
createInitSlottingData psd esd = SlottingData validInitialSlottingData
  where

    validInitialSlottingData :: Map EpochIndex EpochSlottingData
    validInitialSlottingData = M.union currentEpochSlottingData nextEpochSlottingData

    currentEpochSlottingData :: Map EpochIndex CurrentEpochSlottingData
    currentEpochSlottingData = M.singleton 0 psd

    nextEpochSlottingData :: Map EpochIndex NextEpochSlottingData
    nextEpochSlottingData = M.singleton 1 esd

-- | Get all epoch index.
getAllEpochIndex :: SlottingData -> [EpochIndex]
getAllEpochIndex = M.keys . getSlottingDataMap

-- | Get the next epoch index.
getNextEpochIndex :: SlottingData -> EpochIndex
getNextEpochIndex = fst . M.findMax . getSlottingDataMap

-- | Get the next epoch slotting data.
getNextEpochSlottingData :: SlottingData -> EpochSlottingData
getNextEpochSlottingData = snd . M.findMax . getSlottingDataMap

-- | Get the current epoch index. Next epoch - 1.
getCurrentEpochIndex :: SlottingData -> EpochIndex
getCurrentEpochIndex = decreaseEpochIndex . getNextEpochIndex
  where
    decreaseEpochIndex :: EpochIndex -> EpochIndex
    decreaseEpochIndex ei = EpochIndex $ getEpochIndex ei - 1

-- | Get the current epoch slotting data. Next epoch - 1.
getCurrentEpochSlottingData :: SlottingData -> EpochSlottingData
getCurrentEpochSlottingData sdp@(getSlottingDataMap -> sd) = sd M.! currentEpochIndex
  where
    currentEpochIndex = getCurrentEpochIndex sdp

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
      slotTimestamp localSlotIndex' epochSlottingData =
          addTimeDiffToTimestamp epochStartTimeDiff currentSlotTimestamp
        where
          intSlotIndex :: Word16
          intSlotIndex = getSlotIndex localSlotIndex'

          epochStartTimeDiff :: TimeDiff
          epochStartTimeDiff = esdStartDiff epochSlottingData

          epochSlotDuration :: Millisecond
          epochSlotDuration = esdSlotDuration epochSlottingData

          currentSlotTimestamp :: Timestamp
          currentSlotTimestamp =
              Timestamp (fromIntegral intSlotIndex * convertUnit epochSlotDuration)