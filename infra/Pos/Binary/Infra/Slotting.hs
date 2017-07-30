-- | Binary serialization of types from Slotting.

module Pos.Binary.Infra.Slotting
       (
       ) where

import           Universum

import           Data.Time.Units    (Millisecond)
import           Data.Map           as M

import           Pos.Binary.Class   (Cons (..), Field (..), Bi (..), deriveSimpleBi)
import           Pos.Binary.Core    ()
import           Pos.Core.Timestamp (TimeDiff)
import           Pos.Slotting.Types (EpochSlottingData (..), SlottingData, getSlottingDataMap, createSlottingDataUnsafe)

deriveSimpleBi ''EpochSlottingData [
    Cons 'EpochSlottingData [
        Field [| esdSlotDuration :: Millisecond |],
        Field [| esdStartDiff    :: TimeDiff    |]
    ]]

instance Bi SlottingData where
    encode slottingData = encode $ getSlottingDataMap slottingData
    decode = checkIfSlottindDataValid $ decode
      where
        -- We first check if the data we are trying to decode is valid.
        -- We don't want to throw a runtime error.
        checkIfSlottindDataValid slottingDataM = do
            slottingData <- slottingDataM
            if M.size slottingData < 2
                then fail "Invalid slotting data state!"
                else pure $ createSlottingDataUnsafe slottingData

