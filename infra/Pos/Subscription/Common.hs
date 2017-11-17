-- | Common definitions for peer discovery and subscription workers.


module Pos.Subscription.Common
    ( SubscriptionMode
    , SubscriptionTerminationReason (..)
    , subscribeTo
    , subscriptionListener
    , subscriptionListeners
    , subscriptionWorker
    ) where

import qualified Network.Broadcast.OutboundQueue as OQ
import           Network.Broadcast.OutboundQueue.Types (removePeer, simplePeers)
import           Universum hiding (bracket)

import           Formatting (sformat, shown, (%))
import           Mockable (Bracket, Catch, Mockable, Throw, bracket, try)
import           Node.Message.Class (Message)
import           System.Wlog (WithLogger, logDebug, logNotice)

import           Pos.Binary.Class (Bi)
import           Pos.Communication.Limits.Types (MessageLimited, recvLimited)
import           Pos.Communication.Listener (listenerConv)
import           Pos.Communication.Protocol (Conversation (..), ConversationActions (..),
                                             ListenerSpec, MkListeners, MsgSubscribe (..), NodeId,
                                             OutSpecs, SendActions, Worker, WorkerSpec,
                                             constantListeners, convH, toOutSpecs, withConnectionTo,
                                             worker)
import           Pos.DB.Class (MonadGState)
import           Pos.KnownPeers (MonadKnownPeers (..))
import           Pos.Network.Types (Bucket (..), NodeType)
import           Pos.Util.Timer (Timer, startTimer, waitTimer)

type SubscriptionMode m =
    ( MonadIO m
    , WithLogger m
    , Mockable Throw m
    , Mockable Catch m
    , Mockable Bracket m
    , MonadKnownPeers m
    , MonadGState m
    , Message MsgSubscribe
    , MessageLimited MsgSubscribe m
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
    => Timer -> SendActions m -> NodeId -> m SubscriptionTerminationReason
subscribeTo keepAliveTimer sendActions peer = do
    logNotice $ msgSubscribingTo peer
    outcome <- try $ withConnectionTo sendActions peer $ \_peerData -> do
        pure $ Conversation $ \(conv :: ConversationActions MsgSubscribe Void m) -> do
            send conv MsgSubscribe
            forever $ do
                startTimer keepAliveTimer
                atomically $ waitTimer keepAliveTimer
                logDebug $ sformat ("subscriptionWorker: sending keep-alive to "%shown)
                                    peer
                send conv MsgSubscribeKeepAlive
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
subscriptionListener
    :: forall pack m.
       (SubscriptionMode m)
    => OQ.OutboundQ pack NodeId Bucket
    -> NodeType
    -> (ListenerSpec m, OutSpecs)
subscriptionListener oq nodeType = listenerConv @Void oq $ \__ourVerInfo nodeId conv -> do
    recvLimited conv >>= \case
        Just MsgSubscribe -> do
            let peers = simplePeers [(nodeType, nodeId)]
            bracket
              (updatePeersBucket BucketSubscriptionListener (<> peers))
              (\added -> when added $
                void $ updatePeersBucket BucketSubscriptionListener (removePeer nodeId))
              (\added -> when added . fix $ \loop -> recvLimited conv >>= \case
                  Just MsgSubscribeKeepAlive -> do
                      logDebug $ sformat
                          ("subscriptionListener: received keep-alive from "%shown)
                          nodeId
                      loop
                  msg -> logNotice $ expectedMsgFromGot MsgSubscribeKeepAlive
                                                        nodeId msg
              ) -- if not added, close the conversation
        msg -> logNotice $ expectedMsgFromGot MsgSubscribe nodeId msg
  where
    expectedMsgFromGot = sformat
            ("subscriptionListener: expected "%shown%" from "%shown%
             ", got "%shown%", closing the connection")

subscriptionListeners
    :: forall pack m.
       (SubscriptionMode m)
    => OQ.OutboundQ pack NodeId Bucket
    -> NodeType
    -> MkListeners m
subscriptionListeners oq nodeType = constantListeners [subscriptionListener oq nodeType]

-- | Throw the standard subscription worker OutSpecs onto a given
-- implementation of a single subscription worker.
subscriptionWorker
    :: forall m. (SubscriptionMode m)
    => Worker m -> ([WorkerSpec m], OutSpecs)
subscriptionWorker theWorker = first (:[]) (worker subscriptionWorkerSpec theWorker)
  where
    subscriptionWorkerSpec :: OutSpecs
    subscriptionWorkerSpec = toOutSpecs [ convH (Proxy @MsgSubscribe) (Proxy @Void) ]
