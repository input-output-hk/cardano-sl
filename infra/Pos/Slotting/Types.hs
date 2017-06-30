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

import           Data.HashMap.Strict as HM
import           Data.Time.Units     (Millisecond, convertUnit)

import           Pos.Core            (EpochIndex, SlotId (..), Timestamp (..),
                                      getSlotIndex)
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
-- TODO: Reflect the fact that this will always have 2+ elements
-- TODO: Make this a newtype wrapper
type SlottingData = HashMap EpochIndex EpochSlottingData

-- PRIVATE
-- AJ: TODO: GOOD DEFAULT VALUE FOR SLOT DURATION?
ensureEpochSlottingDataExists :: Maybe EpochSlottingData -> EpochSlottingData
ensureEpochSlottingDataExists = fromMaybe $ EpochSlottingData 0 0

-- PRIVATE
-- AJ: TODO: DELETE
-- ensureEpochIndexExists :: Maybe EpochIndex -> EpochIndex
-- ensureEpochIndexExists = fromMaybe 0

-- | Get the latest epoch index.
getLastEpochIndex :: SlottingData -> Maybe EpochIndex
getLastEpochIndex = fmap maximum . nonEmpty . HM.keys

-- | Get the latest epoch slotting data.
getLastEpoch :: SlottingData -> EpochSlottingData
getLastEpoch = ensureEpochSlottingDataExists . fmap (snd . maximumBy (comparing fst)) . nonEmpty . HM.toList

-- | Get the penultimate epoch index.
-- AJ: TODO: Should this return a Maybe??
getPenultEpochIndex :: SlottingData -> EpochIndex
getPenultEpochIndex sd =
  case getLastEpochIndex sd of
    Nothing -> 0
    Just ei -> if ei < 0 then 0 else ei

-- | Get the penultimate epoch slotting data.
getPenultEpoch :: SlottingData -> EpochSlottingData
getPenultEpoch sd = ensureEpochSlottingDataExists $ lookupEpochSlottingData (getPenultEpochIndex sd) sd

-- | Lookup the slotting data for an arbitrary `EpochIndex`.
lookupEpochSlottingData :: EpochIndex -> SlottingData -> Maybe EpochSlottingData
lookupEpochSlottingData = HM.lookup

-- | Insert `EpochSlottingData`.
addEpochSlottingData :: EpochIndex -> EpochSlottingData -> SlottingData -> SlottingData
addEpochSlottingData = HM.insert

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
