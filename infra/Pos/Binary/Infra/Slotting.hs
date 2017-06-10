-- | Binary serialization of types from Slotting.

module Pos.Binary.Infra.Slotting
       (
       ) where

import           Universum

import           Pos.Binary.Class   (Bi (..), combineSize, label)
import           Pos.Binary.Core    ()
import           Pos.Slotting.Types (EpochSlottingData (..), SlottingData (..))

instance Bi EpochSlottingData where
    size = combineSize (esdSlotDuration, esdStart)
    put EpochSlottingData {..} =
        put esdSlotDuration
     *> put esdStart
    get = label "EpochSlottingData" $ do
        esdSlotDuration <- get
        esdStart        <- get
        pure EpochSlottingData{..}

instance Bi SlottingData where
    size = combineSize (sdPenult, sdLast, sdPenultEpoch)
    put SlottingData {..} =
        put sdPenult
     *> put sdLast
     *> put sdPenultEpoch
    get = label "SlottingData" $ do
        sdPenult      <- get
        sdLast        <- get
        sdPenultEpoch <- get
        pure SlottingData{..}
