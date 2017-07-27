{-# LANGUAGE RankNTypes #-}

module Pos.KnownPeers (
    MonadKnownPeers(..)
  , MonadFormatPeers(..)
  ) where

import Universum
import Formatting (Format)
import Pos.Communication.Types.Protocol (NodeId)
import Network.Broadcast.OutboundQueue (Peers)

class MonadKnownPeers m where
  updateKnownPeers :: (Peers NodeId -> Peers NodeId) -> m ()
  addKnownPeers    :: Peers NodeId -> m ()
  removeKnownPeer  :: NodeId -> m ()

-- | For debugging: return formatted list of peers, if available
class MonadFormatPeers m where
  formatKnownPeers :: (forall a . Format r a -> a) -> m (Maybe r)
