-- | Provides functionality of representing `Bi` instances as correct
-- `Message`s used by time-warp.

module Pos.Binary.Infra.DHTModel () where

import           Universum

import           Network.Kademlia.HashNodeId (HashId (..))

import           Pos.Binary.Class            (Bi (..), Size (..), getSize, label)
import           Pos.DHT.Model.Types         (DHTData (..), DHTKey (..))

instance Bi DHTKey where
    -- CSL-1122: is this a constant?
    size = VarSize $ \(DHTKey (HashId bs)) -> getSize bs
    put (DHTKey (HashId bs)) = put bs
    get = label "DHTKey" $ DHTKey . HashId <$> get

instance Bi DHTData where
    size = ConstSize 0
    put (DHTData ()) = pure ()
    get = pure $ DHTData ()
