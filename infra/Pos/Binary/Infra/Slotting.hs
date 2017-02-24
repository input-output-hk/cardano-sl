-- | Binary serialization of types from Slotting.

module Pos.Binary.Infra.Slotting
       (
       ) where

import           Data.Binary.Get    (getWord8, label)
import           Universum

import           Pos.Binary.Class   (Bi (..), UnsignedVarInt (..))
import           Pos.Core.Types     (EpochIndex (..), Timestamp (..))
import           Pos.Slotting.Types (EpochSlottingData (..), SlottingData (..))

instance Bi Timestamp where
    get = label "Timestamp" $ fromInteger <$> get
    put = put . toInteger

instance Bi EpochIndex where
    get = label "EpochIndex" $ EpochIndex . getUnsignedVarInt <$> get
    put (EpochIndex c) = put (UnsignedVarInt c)

instance Bi EpochSlottingData where
    put EpochSlottingData {..} = do
        put esdSlotDuration
        put esdStart
    get = do
        esdSlotDuration <- get
        esdStart <- get
        return EpochSlottingData {..}

instance Bi SlottingData where
    put SlottingData {..} = do
        put sdPenult
        put sdLast
        put sdPenultEpoch
    get = do
        sdPenult <- get
        sdLast <- get
        sdPenultEpoch <- get
        return SlottingData {..}
