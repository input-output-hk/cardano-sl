-- | Binary serialization of types from Slotting.

module Pos.Binary.Infra.Slotting
       (
       ) where

import           Data.Time.Units    (Millisecond)

import           Pos.Binary.Class   (Cons (..), Field (..), deriveSimpleBi)
import           Pos.Binary.Core    ()
import           Pos.Core.Timestamp (Timestamp)
import           Pos.Core.Types     (EpochIndex)
import           Pos.Slotting.Types (EpochSlottingData (..), SlottingData (..))

deriveSimpleBi ''EpochSlottingData [
    Cons 'EpochSlottingData [
        Field [| esdSlotDuration :: Millisecond |],
        Field [| esdStartDiff    :: Timestamp   |]
    ]]

-- CSL-1122: add a test for serialization of 'SlottingData'
deriveSimpleBi ''SlottingData [
    Cons 'SlottingData [
        Field [| sdPenult      :: EpochSlottingData |],
        Field [| sdLast        :: EpochSlottingData |],
        Field [| sdPenultEpoch :: EpochIndex        |]
    ]]
