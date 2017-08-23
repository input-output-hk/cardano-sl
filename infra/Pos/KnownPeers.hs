{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

module Pos.KnownPeers (
    MonadKnownPeers(..)
  , HasOutboundQ(..)
  , resetFailureStatus
  , MonadFormatPeers(..)
  ) where

import           Control.Monad.Trans.Class
import           Formatting                       (Format)
import           Network.Broadcast.OutboundQueue  (OutboundQ, Peers, clearFailureOf)
import           Pos.Communication.Types.Protocol (NodeId)
import           Pos.Network.Types                (Bucket)
import           Universum

class MonadKnownPeers m where
  updatePeersBucket :: Bucket -> (Peers NodeId -> Peers NodeId) -> m Bool

instance {-# OVERLAPPABLE #-}
    ( Monad m, MonadTrans f, MonadKnownPeers m ) =>
        MonadKnownPeers (f m) where
    updatePeersBucket bucket = lift . updatePeersBucket bucket

-- | For debugging: return formatted list of peers, if available
class MonadFormatPeers m where
  formatKnownPeers :: (forall a . Format r a -> a) -> m (Maybe r)

instance {-# OVERLAPPABLE #-}
    ( Monad m, MonadTrans f, MonadFormatPeers m ) =>
        MonadFormatPeers (f m) where
  formatKnownPeers k = lift (formatKnownPeers k)

class HasOutboundQ m where
  type Pack m :: * -> *
  getOutboundQ :: m (OutboundQ (Pack m) NodeId Bucket)

resetFailureStatus :: (MonadIO m, HasOutboundQ m) => NodeId -> m ()
resetFailureStatus nid = flip clearFailureOf nid =<< getOutboundQ
