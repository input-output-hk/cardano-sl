{-# LANGUAGE UndecidableInstances #-}

-- | Provides functionality of representing `Bi` instances as correct
-- `Message`s used by time-warp.

module Pos.Binary.DHTModel () where

import qualified Data.Binary                 as B (Binary (..))
import           Data.Binary.Get             (label)
import           Network.Kademlia.HashNodeId (HashId (..))
import           Universum

import           Pos.Binary.Class            (Bi (..))
import           Pos.DHT.Model.Types         (DHTData (..), DHTKey (..))

instance Bi DHTKey where
    put (DHTKey (HashId bs)) = put bs
    get = label "DHTKey" $ DHTKey . HashId <$> get

instance Bi DHTData where
    put (DHTData ()) = mempty
    get = pure $ DHTData ()

instance B.Binary DHTKey where
    get = get
    put = put
