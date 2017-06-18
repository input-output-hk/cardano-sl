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

instance Bi SlottingData where
    sizeNPut = labelS "SlottingData" $
        putField sdPenult <>
        putField sdLast <>
        putField sdPenult
    get = label "SlottingData" $ do
        sdPenult      <- get
        sdLast        <- get
        sdPenultEpoch <- get
        pure SlottingData{..}
