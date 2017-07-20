module Pos.KnownPeers (
    MonadKnownPeers(..)
  ) where

import Pos.Communication.Protocol (NodeId)
import Network.Broadcast.OutboundQueue (Peers)

class MonadKnownPeers m where
  updateKnownPeers :: (Peers NodeId -> Peers NodeId) -> m ()
  addKnownPeers    :: Peers NodeId -> m ()
  removeKnownPeer  :: NodeId -> m ()
