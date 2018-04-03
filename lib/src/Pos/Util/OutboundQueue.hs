{-# LANGUAGE RankNTypes #-}

-- | Utilities related to the OutboundQueue and its place here in cardano-sl.

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

-- | Specialization of OutboundQ for use in cardano-sl. Peers are identified
-- by 'NodeId' and the data to be enqueued is an 'EnqueuedConversation'.
type OQ = OQ.OutboundQ EnqueuedConversation NodeId Bucket

newtype EnqueuedConversation t =
    EnqueuedConversation (Msg, NodeId -> PeerData -> Conversation PackingType t)

instance OQ.FormatMsg EnqueuedConversation where
    formatMsg = (\k (EnqueuedConversation (msg, _)) -> k msg) <$> shown

updatePeersBucketReader
    :: ( MonadReader r m, MonadIO m )
    => (r -> OQ)
    -> Bucket
    -> (OQ.Peers NodeId -> OQ.Peers NodeId)
    -> m Bool
updatePeersBucketReader pick buck f = asks pick >>= updateBucket
  where
    updateBucket oq = liftIO $ OQ.updatePeersBucket oq buck f

formatKnownPeersReader
    :: ( MonadReader r m, MonadIO m )
    => (r -> OQ)
    -> (forall a . Format t a -> a)
    -> m (Maybe t)
formatKnownPeersReader pick formatter = asks pick >>= dumpFormattedState
  where
    dumpFormattedState oq = fmap Just (liftIO $ OQ.dumpState oq formatter)
