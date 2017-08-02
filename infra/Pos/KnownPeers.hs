{-# LANGUAGE RankNTypes #-}

module Pos.KnownPeers (
    MonadKnownPeers(..)
  , MonadFormatPeers(..)
  ) where

import Universum
import Formatting (Format)
import Pos.Communication.Types.Protocol (NodeId)
import Pos.Network.Types (Bucket)
import Network.Broadcast.OutboundQueue (Peers)

class MonadKnownPeers m where
  updatePeersBucket :: Bucket -> (Peers NodeId -> Peers NodeId) -> m ()

-- | For debugging: return formatted list of peers, if available
class MonadFormatPeers m where
  formatKnownPeers :: (forall a . Format r a -> a) -> m (Maybe r)
