{-# LANGUAGE UndecidableInstances #-}

-- | Provides functionality of representing `Bi` instances as correct
-- `Message`s used by time-warp.

module Pos.Binary.DHTModel () where

import           Data.Binary.Get             (getWord8)
import           Data.Binary.Put             (putWord8)
import           Universum

import           Pos.Binary.Class            (Bi (..))
import           Pos.DHT.Model.Class         (DHTMsgHeader (..))
import           Pos.DHT.Model.Types         (DHTData (..), DHTKey (..))

import           Network.Kademlia.HashNodeId (HashId (..))

instance Bi DHTMsgHeader where
    put BroadcastHeader  = putWord8 0
    put (SimpleHeader b) = putWord8 1 >> put b
    get = getWord8 >>= \case
        0 -> pure BroadcastHeader
        1 -> SimpleHeader <$> get
        tag -> fail ("get@DHTMsgHeader: invalid tag: " ++ show tag)

instance Bi DHTKey where
    put (DHTKey (HashId bs)) = put bs
    get = DHTKey . HashId <$> get

instance Bi DHTData where
    put (DHTData ()) = mempty
    get = pure $ DHTData ()
