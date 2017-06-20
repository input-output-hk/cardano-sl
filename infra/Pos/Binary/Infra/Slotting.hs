-- | Binary serialization of types from Slotting.

module Pos.Binary.Infra.Slotting
       (
       ) where

import           Universum

import           Pos.Binary.Class   (Bi (..), label, labelS, putField)
import           Pos.Binary.Core    ()
import           Pos.Slotting.Types (EpochSlottingData (..), SlottingData (..))

instance Bi EpochSlottingData where
    sizeNPut = labelS "EpochSlottingData" $
        putField esdSlotDuration <>
        putField esdStart
    get = label "EpochSlottingData" $ do
        esdSlotDuration <- get
        esdStart        <- get
        pure EpochSlottingData{..}

-- CSL-1122: add a test for serialization of 'SlottingData'
instance Bi SlottingData where
    sizeNPut = labelS "SlottingData" $
        putField sdPenult <>
        putField sdLast <>
        putField sdPenultEpoch
    get = label "SlottingData" $ do
        sdPenult      <- get
        sdLast        <- get
        sdPenultEpoch <- get
        pure SlottingData{..}
