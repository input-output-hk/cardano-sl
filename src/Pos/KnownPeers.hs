module Pos.KnownPeers (
    MonadKnownPeers(..)
  ) where

import Pos.Communication.Protocol (NodeId)
import Network.Broadcast.OutboundQueue (Peers)

class MonadKnownPeers m where
  addKnownPeers   :: Peers NodeId -> m ()
  removeKnownPeer :: NodeId -> m ()
