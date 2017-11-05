-- | Binary serialization of types from Slotting.

module Pos.Binary.Infra.Slotting
       (
       ) where

import           Universum

import           Data.Time.Units    (Millisecond)

import           Pos.Binary.Class   (Bi (..), Cons (..), Field (..), deriveSimpleBi)
import           Pos.Binary.Core    ()
import           Pos.Core.Timestamp (TimeDiff)
import           Pos.Slotting.Types (EpochSlottingData (..), SlottingData,
                                     createSlottingDataUnsafe, getSlottingDataMap,
                                     isValidSlottingDataMap)

deriveSimpleBi ''EpochSlottingData [
    Cons 'EpochSlottingData [
        Field [| esdSlotDuration :: Millisecond |],
        Field [| esdStartDiff    :: TimeDiff    |]
    ]]

instance Bi SlottingData where
    encode slottingData = encode $ getSlottingDataMap slottingData
    decode = checkIfSlottindDataValid $ decode
      where
        -- We first check if the data we are trying to decode is
        -- valid. We don't want to throw a runtime error. dcNoCheck
        -- is not used here because we don't store SlottingData in db.
        checkIfSlottindDataValid slottingDataM = do
            slottingData <- slottingDataM
            unless (isValidSlottingDataMap slottingData) $
                fail "Invalid slotting data state!"
            pure $ createSlottingDataUnsafe slottingData
