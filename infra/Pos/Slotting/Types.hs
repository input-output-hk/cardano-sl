-- | Core types used in 'Slotting'.

module Pos.Slotting.Types
       ( EpochSlottingData (..)
       , SlottingData
       , getSlottingDataMap
       , createSlottingDataUnsafe
       , isValidSlottingDataMap
       , createInitSlottingData
       , getAllEpochIndices
       , getCurrentEpochIndex
       , getCurrentEpochSlottingData
       , getNextEpochIndex
       , getNextEpochSlottingData
       , insertEpochSlottingDataUnsafe
       , addEpochSlottingData
       , lookupEpochSlottingData
       , computeSlotStart
       ) where

import           Universum

import           Data.Map.Strict as M
import           Data.Time.Units (Millisecond, toMicroseconds)

import           Pos.Core (EpochIndex (..), LocalSlotIndex (..), TimeDiff (..), Timestamp (..),
                           addTimeDiffToTimestamp, getSlotIndex)
import           Pos.Util.Util ()


----------------------------------------------------------------------------
-- Type declarations
----------------------------------------------------------------------------

-- | Data which is necessary for slotting and corresponds to a particular epoch.
data EpochSlottingData = EpochSlottingData
    { esdSlotDuration :: !Millisecond
    -- ^ Slot duration for a specific epoch.
    , esdStartDiff    :: !TimeDiff
    -- ^ Difference between epoch start and system start time.
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

----------------------------------------------------------------------------
-- Functions
----------------------------------------------------------------------------

-- | Unsafe constructor that can lead to unsafe crash!
createSlottingDataUnsafe :: Map EpochIndex EpochSlottingData -> SlottingData
createSlottingDataUnsafe epochSlottingDataMap =
    if isValidSlottingDataMap epochSlottingDataMap
        then SlottingData epochSlottingDataMap
        else criticalError
  where
    criticalError = error "It's impossible to create slotting data without at least\
    \ two epochs. Epochs need to be sequential."

-- | The validation for the @SlottingData@. It's visible since it's needed externally.
isValidSlottingDataMap :: Map EpochIndex EpochSlottingData -> Bool
isValidSlottingDataMap epochSlottingDataMap =
    M.size epochSlottingDataMap >= 2 && validEpochIndices
  where
    -- We validate if the epoch indices are sequential, it's invalid if they
    -- start having "holes" [..,6,7,9,...].
    validEpochIndices = correctEpochIndices == currentEpochIndices
      where
        currentEpochIndices = keys epochSlottingDataMap
        correctEpochIndices = EpochIndex . fromIntegral <$> [0..zIMapLenght]
          where
            zIMapLenght = pred . length . keys $ epochSlottingDataMap

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
getAllEpochIndices :: SlottingData -> [EpochIndex]
getAllEpochIndices = M.keys . getSlottingDataMap

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
    -- Left for readability.
    decreaseEpochIndex :: EpochIndex -> EpochIndex
    decreaseEpochIndex = pred

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

-- | Insert `EpochSlottingData`. This is not a really good idea, we would prefer
-- @addEpochSlottingData@.
insertEpochSlottingDataUnsafe
    :: EpochIndex
    -> EpochSlottingData
    -> SlottingData
    -> SlottingData
insertEpochSlottingDataUnsafe epochIndex epochSlottingData slottingData =
    SlottingData $ M.insert epochIndex epochSlottingData slottingData'
  where
    slottingData' :: Map EpochIndex EpochSlottingData
    slottingData' = getSlottingDataMap slottingData

-- | Add `EpochSlottingData`.
addEpochSlottingData :: SlottingData -> EpochSlottingData -> SlottingData
addEpochSlottingData slottingData epochSlottingData =
    SlottingData $ M.insert nextEpochIndex epochSlottingData slottingData'
  where
    -- We can calculate the index ourselves, no need to pass it around
    nextEpochIndex :: EpochIndex
    nextEpochIndex = EpochIndex . succ . getEpochIndex . getNextEpochIndex $ slottingData

    slottingData' :: Map EpochIndex EpochSlottingData
    slottingData' = getSlottingDataMap slottingData

-- | Compute when the slot started. We give it @LocalSlotIndex@,
-- @EpochSlottingData@ and find when did that @LocalSlotIndex@ occur.
-- This is calculating times inside an @Epoch@.
-- Note that the time here has to be in @Microseconds@.
computeSlotStart :: Timestamp -> LocalSlotIndex -> EpochSlottingData -> Timestamp
computeSlotStart systemStart slotIndex epochSlottingData =
    epochStartTime + currentSlotTimestamp
  where
    -- | We get the epoch start time by adding the epoch slotting data start diff
    -- which is:
    --   currentEpochStart - systemStart + systemStart = currentEpochStart
    epochStartTime :: Timestamp
    epochStartTime = addTimeDiffToTimestamp epochStartTimeDiff systemStart
      where
        epochStartTimeDiff :: TimeDiff
        epochStartTimeDiff = esdStartDiff epochSlottingData

    -- | We calculate the current slot @Timestamp@ - when did the current slot start.
    currentSlotTimestamp :: Timestamp
    currentSlotTimestamp = Timestamp . fromIntegral $ slotStartTime
      where
        -- | Start time in microseconds as @Timestamp@ is.
        slotStartTime :: Integer
        slotStartTime = fromIntegral intSlotIndex * epochSlotDuration
          where
            -- | In microseconds.
            epochSlotDuration :: Integer
            epochSlotDuration = toMicroseconds . esdSlotDuration $ epochSlottingData

            -- | The slot index in an @Epoch@.
            intSlotIndex :: Word16
            intSlotIndex = getSlotIndex slotIndex
