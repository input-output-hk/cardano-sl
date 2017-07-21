-- | Core types used in 'Slotting'.

module Pos.Slotting.Types
       ( EpochSlottingData (..)
       , SlottingData
       , getLastEpochIndex
       , getPenultEpochIndex
       , getLastEpoch
       , getPenultEpoch
       , addEpochSlottingData
       , lookupEpochSlottingData
       , computeSlotStart
       , findMatchingEpoch
       ) where

import           Universum

import           Data.Map.Strict     as M
import           Data.Time.Units     (Millisecond, convertUnit)

import           Pos.Core            (EpochIndex, SlotId (..), Timestamp (..), 
                                     EpochIndex(..), getSlotIndex)
import           Pos.Util.Util       ()

-- | Data which is necessary for slotting and corresponds to a particular epoch.
data EpochSlottingData = EpochSlottingData
    { esdSlotDuration :: !Millisecond
    -- ^ Slot duration actual for given epoch.
    , esdStart        :: !Timestamp
    -- ^ Time when epoch starts.
    } deriving (Eq, Show, Generic)

instance NFData EpochSlottingData

-- | Data necessary for slotting to work which is basically part of GState.
-- External code can use functions like getLastEpochIndex or getLastEpochSlottingData and 
-- not worry about cases where it doesn't exist (this module should be responsible for it).
-- Note that it's important to use error rather than default values like 0, because 
-- such cases indicate invariants violation and shouldn't be hidden behind default values.
type SlottingData = Map EpochIndex EpochSlottingData

-- | Get the latest epoch index.
getLastEpochIndex :: SlottingData -> EpochIndex
getLastEpochIndex = fst . M.findMax

-- | Get the latest epoch slotting data.
getLastEpoch :: SlottingData -> EpochSlottingData
getLastEpoch = snd . M.findMax

-- | Get the penultimate epoch index. Last epoch - 1.
getPenultEpochIndex :: SlottingData -> EpochIndex
getPenultEpochIndex sd = decreaseEpochIndex $ getLastEpochIndex sd
  where
    decreaseEpochIndex :: EpochIndex -> EpochIndex
    decreaseEpochIndex ei = EpochIndex $ getEpochIndex ei - 1

-- | Get the penultimate epoch slotting data. Last epoch - 1.
getPenultEpoch :: SlottingData -> EpochSlottingData
getPenultEpoch sd = sd ! getPenultEpochIndex sd

-- | Lookup the slotting data for an arbitrary `EpochIndex`.
lookupEpochSlottingData :: EpochIndex -> SlottingData -> Maybe EpochSlottingData
lookupEpochSlottingData = M.lookup

-- | Insert `EpochSlottingData`.
addEpochSlottingData :: EpochIndex -> EpochSlottingData -> SlottingData -> SlottingData
addEpochSlottingData = M.insert

computeSlotStart :: SlotId -> EpochSlottingData -> Timestamp
computeSlotStart SlotId{..} esd = slotTimestamp siSlot esd
    where
      slotTimestamp (getSlotIndex -> locSlot) EpochSlottingData{..} =
        esdStart + Timestamp (fromIntegral locSlot * convertUnit esdSlotDuration)

findMatchingEpoch :: Monad m => Timestamp -> EpochIndex -> (EpochIndex -> m (Maybe EpochSlottingData)) -> m (Maybe (EpochIndex, EpochSlottingData))
findMatchingEpoch approxCurrentTime lastEpochIndex getterEpochSlottingData = go lastEpochIndex
  -- Iterate over lastEpochIndex..0 and find the epoch
  where
    go ei
      | ei < 0 = return Nothing
      | otherwise = do
          mesd <- getterEpochSlottingData ei
          case mesd of
            Nothing -> return Nothing
            Just esd -> do
              if esdStart esd < approxCurrentTime
                then return $ Just (ei, esd)
                else go (ei - 1)
