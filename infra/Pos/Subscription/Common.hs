-- | Common defintiions for definition peer discovery and subscription workers.

{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Subscription.Common
    ( SubscriptionMode
    , SubscriptionTerminationReason (..)
    , subscribeTo
    , subscriptionListener
    , subscriptionListeners
    , subscriptionWorker
    ) where

import           Universum                  hiding (bracket_)
import           Network.Broadcast.OutboundQueue.Types (simplePeers)

import           Formatting                 (sformat, shown, (%))
import           System.Wlog                (WithLogger, logNotice)
import           Mockable                   (Mockable, Delay, Throw, Catch, Bracket,
                                             try, bracket_)
import           Node.Message.Class         (Message)

import           Pos.Binary.Class           (Bi)
import           Pos.Communication.Protocol (OutSpecs, WorkerSpec, MsgSubscribe (..),
                                             Conversation (..), ConversationActions (..),
                                             convH, toOutSpecs, Worker, worker,
                                             ListenerSpec, NodeId, withConnectionTo,
                                             SendActions, MkListeners, constantListeners)
import           Pos.Communication.Listener (listenerConv)
import           Pos.Communication.Limits.Types (MessageLimited, recvLimited)
import           Pos.DB.Class               (MonadGState)
import           Pos.KnownPeers             (MonadKnownPeers(..))
import           Pos.Network.Types          (NodeType)

type SubscriptionMode m =
    ( MonadIO m
    , WithLogger m
    , Mockable Throw m
    , Mockable Catch m
    , Mockable Bracket m
    , MonadKnownPeers m
    , MonadGState m
    , Message MsgSubscribe
    , MessageLimited MsgSubscribe
    , Bi MsgSubscribe
    , Message Void
    )

-- | A subscription ends normally (remote shut it down) or exceptionally
-- (network issues etc.).
data SubscriptionTerminationReason =
      Normal
    | Exceptional SomeException
    deriving (Show)

-- | Subscribe to some peer, blocking until the subscription terminates and
-- giving the reason. Notices will be logged before and after the subscription.
subscribeTo
    :: forall m. (SubscriptionMode m)
    => SendActions m -> NodeId -> m SubscriptionTerminationReason
subscribeTo sendActions peer = do
    logNotice $ msgSubscribingTo peer
    outcome <- try $ withConnectionTo sendActions peer $ \_peerData ->
           pure $ Conversation $ \conv -> do
               send conv MsgSubscribe
               recv conv 0 :: m (Maybe Void) -- Other side will never send
    let reason = either Exceptional (maybe Normal absurd) outcome
    logNotice $ msgSubscriptionTerminated peer reason
    return reason
  where
    msgSubscribingTo :: NodeId -> Text
    msgSubscribingTo = sformat $ "subscriptionWorker: subscribing to "%shown

    msgSubscriptionTerminated :: NodeId -> SubscriptionTerminationReason -> Text
    msgSubscriptionTerminated = sformat $ "subscriptionWorker: lost connection to "%shown%" "%shown

-- | A listener for subscriptions: add the subscriber to the set of known
-- peers, annotating it with a given NodeType. Remove that peer from the set
-- of known peers when the connection is dropped.
--
-- FIXME this may clash with the subscription worker, which may also modify
-- the known peers.
subscriptionListener
    :: forall m.
       (SubscriptionMode m)
    => NodeType
    -> (ListenerSpec m, OutSpecs)
subscriptionListener nodeType = listenerConv @Void $ \__ourVerInfo nodeId conv -> do
    mbMsg <- recvLimited conv
    whenJust mbMsg $ \MsgSubscribe -> do
      let peers = simplePeers [(nodeType, nodeId)]
      bracket_ (addKnownPeers peers)
                 (removeKnownPeer nodeId)
                 (void $ recvLimited conv)

subscriptionListeners
    :: forall m.
       (SubscriptionMode m)
    => NodeType
    -> MkListeners m
subscriptionListeners nodeType = constantListeners [subscriptionListener nodeType]

-- | Throw the standard subscription worker OutSpecs onto a given
-- implementation of a single subscription worker.
subscriptionWorker
    :: forall m. (SubscriptionMode m)
    => Worker m -> ([WorkerSpec m], OutSpecs)
subscriptionWorker theWorker = first (:[]) (worker subscriptionWorkerSpec theWorker)
  where
    subscriptionWorkerSpec :: OutSpecs
    subscriptionWorkerSpec = toOutSpecs [ convH (Proxy @MsgSubscribe) (Proxy @Void) ]
