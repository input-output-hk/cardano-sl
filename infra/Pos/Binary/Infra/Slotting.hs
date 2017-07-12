-- | Binary serialization of types from Slotting.

module Pos.Binary.Infra.Slotting
       (
       ) where

import           Data.Time.Units    (Millisecond)

import           Pos.Binary.Class   (Cons (..), Field (..), deriveSimpleBi)
import qualified Pos.Binary.Cbor    as Cbor
import           Pos.Binary.Core    ()
import           Pos.Core.Timestamp (Timestamp)
import           Pos.Core.Types     (EpochIndex)
import           Pos.Slotting.Types (EpochSlottingData (..), SlottingData (..))

deriveSimpleBi ''EpochSlottingData [
    Cons 'EpochSlottingData [
        Field [| esdSlotDuration :: Millisecond |],
        Field [| esdStart        :: Timestamp   |]
    ]]

Cbor.deriveSimpleBi ''EpochSlottingData [
    Cbor.Cons 'EpochSlottingData [
        Cbor.Field [| esdSlotDuration :: Millisecond |],
        Cbor.Field [| esdStart        :: Timestamp   |]
    ]]

-- CSL-1122: add a test for serialization of 'SlottingData'
deriveSimpleBi ''SlottingData [
    Cons 'SlottingData [
        Field [| sdPenult      :: EpochSlottingData |],
        Field [| sdLast        :: EpochSlottingData |],
        Field [| sdPenultEpoch :: EpochIndex        |]
    ]]

Cbor.deriveSimpleBi ''SlottingData [
    Cbor.Cons 'SlottingData [
        Cbor.Field [| sdPenult      :: EpochSlottingData |],
        Cbor.Field [| sdLast        :: EpochSlottingData |],
        Cbor.Field [| sdPenultEpoch :: EpochIndex        |]
    ]]
