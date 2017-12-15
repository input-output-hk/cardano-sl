-- | Utilities related to the OutboundQueue (time-warp-nt) and its place here
-- in cardano-sl.

{-# LANGUAGE RankNTypes #-}

module Pos.Util.OutboundQueue
    ( OQ
    , EnqueuedConversation (..)
    , updatePeersBucketReader
    , formatKnownPeersReader
    ) where

import           Control.Monad.Reader
import           Formatting (Format, shown)
import           Universum

import qualified Network.Broadcast.OutboundQueue as OQ
import qualified Network.Broadcast.OutboundQueue.Types as OQ
import           Node.Conversation (Conversation)
import           Pos.Communication (Msg, NodeId, PackingType, PeerData)
import           Pos.Network.Types (Bucket)
import           System.Wlog.CanLog (WithLogger)

-- | Specialization of OutboundQ for use in cardano-sl. Peers are identified
-- by 'NodeId' and the data to be enqueued is an 'EnqueuedConversation m'.
type OQ m = OQ.OutboundQ (EnqueuedConversation m) NodeId Bucket

newtype EnqueuedConversation m t =
    EnqueuedConversation (Msg, NodeId -> PeerData -> Conversation PackingType m t)

instance OQ.FormatMsg (EnqueuedConversation m) where
    formatMsg = (\k (EnqueuedConversation (msg, _)) -> k msg) <$> shown

updatePeersBucketReader
    :: ( MonadReader r m, MonadIO m, WithLogger m )
    => (r -> OQ n)
    -> Bucket
    -> (OQ.Peers NodeId -> OQ.Peers NodeId)
    -> m Bool
updatePeersBucketReader pick buck f = asks pick >>= updateBucket
  where
    updateBucket oq = OQ.updatePeersBucket oq buck f

formatKnownPeersReader
    :: ( MonadReader r m, MonadIO m )
    => (r -> OQ n)
    -> (forall a . Format t a -> a)
    -> m (Maybe t)
formatKnownPeersReader pick formatter = asks pick >>= dumpFormattedState
  where
    dumpFormattedState oq = fmap Just (OQ.dumpState oq formatter)
