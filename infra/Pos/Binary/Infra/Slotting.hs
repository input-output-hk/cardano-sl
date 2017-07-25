-- | Binary serialization of types from Slotting.

module Pos.Binary.Infra.Slotting
       (
       ) where

import           Universum

import           Data.Time.Units    (Millisecond)

import           Pos.Binary.Class   (Cons (..), Field (..), Bi (..), Size( VarSize ), deriveSimpleBi, getSize)
import           Pos.Binary.Core    ()
import           Pos.Core.Timestamp (TimeDiff)
import           Pos.Slotting.Types (EpochSlottingData (..), SlottingData, getSlottingDataMap, createSlottingDataUnsafe)

deriveSimpleBi ''EpochSlottingData [
    Cons 'EpochSlottingData [
        Field [| esdSlotDuration :: Millisecond |],
        Field [| esdStartDiff    :: TimeDiff    |]
    ]]

-- CSL-1122: add a test for serialization of 'SlottingData'
-- deriveSimpleBi ''SlottingData [
--     Cons 'SlottingData [
--         Field [| getSlottingDataMap :: Map EpochIndex EpochSlottingData |]
--     ]]
instance Bi SlottingData where
    size = VarSize $ \slottingData -> getSize $ getSlottingDataMap slottingData
    put slottingData = put $ getSlottingDataMap slottingData
    get = createSlottingDataUnsafe <$> get


-- newtype SlottingData = SlottingData 
--     { getSlottingDataMap :: Map EpochIndex EpochSlottingData 
--     } deriving (Eq, Show, Generic, Monoid)

-- instance NFData SlottingData

-- -- | Unsafe constructor that can lead to invalid state!
-- createSlottingDataUnsafe :: Map EpochIndex EpochSlottingData -> SlottingData
-- createSlottingDataUnsafe = SlottingData