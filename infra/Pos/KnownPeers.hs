{-# LANGUAGE RankNTypes #-}

module Pos.KnownPeers (
    MonadKnownPeers(..)
  ) where

import Data.Monoid ((<>))
import Formatting (Format)
import Pos.Communication.Types.Protocol (NodeId)
import Network.Broadcast.OutboundQueue (Peers)

class MonadKnownPeers m where
  updateKnownPeers :: (Peers NodeId -> Peers NodeId) -> m ()
  addKnownPeers    :: Peers NodeId -> m ()
  removeKnownPeer  :: NodeId -> m ()
  -- | Good for debugging purposes.
  formatKnownPeers :: (forall a . Format r a -> a) -> m r
