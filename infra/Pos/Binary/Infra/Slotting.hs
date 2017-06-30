-- | Binary serialization of types from Slotting.

module Pos.Binary.Infra.Slotting
       (
       ) where

import           Universum

import           Pos.Binary.Class   (Bi (..))
import           Pos.Binary.Core    ()
import           Pos.Slotting.Types (EpochSlottingData (..))

instance Bi EpochSlottingData where
    put EpochSlottingData {..} = do
        put esdSlotDuration
        put esdStart
    get = do
        esdSlotDuration <- get
        esdStart <- get
        return EpochSlottingData {..}
