{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Binary serialization of types from Slotting.

module Pos.Binary.Infra.Slotting
       (
       ) where

import           Universum

import           Data.Time.Units (Millisecond)

import           Pos.Binary.Class (Bi (..), Cons (..), Field (..), deriveSimpleBi)
import           Pos.Binary.Core ()
import           Pos.Core.Slotting (TimeDiff)
import           Pos.Slotting.Types (EpochSlottingData (..), SlottingData, createSlottingDataUnsafe,
                                     getSlottingDataMap, isValidSlottingDataMap)
import           Pos.Util.Util (cborError)

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
            if isValidSlottingDataMap slottingData
                then pure $ createSlottingDataUnsafe slottingData
                else cborError "Invalid slotting data state!"
