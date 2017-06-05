-- | Provides functionality of representing `Bi` instances as correct
-- `Message`s used by time-warp.

module Pos.Binary.Infra.DHTModel () where

import qualified Data.Binary                 as B (Binary (..))
import           Network.Kademlia.HashNodeId (HashId (..))
import           Universum

import           Pos.Binary.Class            (Bi (..), Size (..), label)
import           Pos.DHT.Model.Types         (DHTData (..), DHTKey (..))

instance B.Binary DHTKey where
    put (DHTKey (HashId bs)) = B.put bs
    get = DHTKey . HashId <$> B.get

instance Bi DHTKey where
    put (DHTKey (HashId bs)) = put bs
    get = label "DHTKey" $ DHTKey . HashId <$> get

instance Bi DHTData where
    size = ConstSize 0
    put (DHTData ()) = pure ()
    get = pure $ DHTData ()
