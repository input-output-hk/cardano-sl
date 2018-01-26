-- | Common definitions for peer discovery and subscription workers.

module Pos.Subscription.Common
    ( SubscriptionMode
    , SubscriptionTerminationReason (..)
    , subscribeTo
    , subscriptionListeners
    , subscriptionWorker
    ) where

import           Universum

import           Control.Exception.Safe (try)
import qualified Data.List.NonEmpty as NE
import qualified Network.Broadcast.OutboundQueue as OQ
import           Network.Broadcast.OutboundQueue.Types (removePeer, simplePeers)

import           Formatting (sformat, shown, (%))
import           Node.Message.Class (Message)
import           System.Wlog (WithLogger, logDebug, logNotice)

import           Pos.Binary.Class (Bi)
import           Pos.Communication.Limits.Types (MessageLimited, recvLimited)
import           Pos.Communication.Listener (listenerConv)
import           Pos.Communication.Protocol (Conversation (..), ConversationActions (..),
                                             ListenerSpec, MkListeners, MsgSubscribe (..),
                                             MsgSubscribe1 (..), NodeId, OutSpecs, SendActions,
                                             Worker, WorkerSpec, constantListeners, convH,
                                             toOutSpecs, withConnectionTo, worker)
import           Pos.KnownPeers (MonadKnownPeers (..))
import           Pos.Network.Types (Bucket (..), NodeType)
import           Pos.Util.Timer (Timer, setTimerDuration, startTimer, waitTimer)

type SubscriptionMode m =
    ( MonadIO m
    , WithLogger m
    , MonadMask m
    , MonadKnownPeers m
    , Message MsgSubscribe
    , Message MsgSubscribe1
    , MessageLimited MsgSubscribe m
    , MessageLimited MsgSubscribe1 m
    , Bi MsgSubscribe
    , Bi MsgSubscribe1
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
    outcome <- try $ withConnectionTo sendActions peer $ \_peerData -> NE.fromList
        -- Sort conversations in descending order based on their version so that
        -- the highest available version of the conversation is picked.
        [ Conversation convMsgSubscribe
        , Conversation convMsgSubscribe1
        ]
    let reason = either Exceptional (maybe Normal absurd) outcome
    logNotice $ msgSubscriptionTerminated peer reason
    return reason
  where
    convMsgSubscribe :: ConversationActions MsgSubscribe Void m -> m t
    convMsgSubscribe conv = do
        send conv MsgSubscribe
        forever $ do
            startTimer keepAliveTimer
            atomically $ waitTimer keepAliveTimer
            logDebug $ sformat ("subscriptionWorker: sending keep-alive to "%shown)
                                peer
            send conv MsgSubscribeKeepAlive
            -- If there is a suspicion that subscriptions are no longer valid,
            -- we want to start sending keep-alive packets more frequently. Use
            -- 20 seconds as we don't have access to slot duration here.
            setTimerDuration keepAliveTimer $ 20 * 1000000

    convMsgSubscribe1 :: ConversationActions MsgSubscribe1 Void m -> m (Maybe Void)
    convMsgSubscribe1 conv = do
        send conv MsgSubscribe1
        recv conv 0 -- Other side will never send

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
subscriptionListener oq nodeType = listenerConv @Void oq $ \_ourVerInfo nodeId conv -> do
    recvLimited conv >>= \case
        Just MsgSubscribe -> do
            let peers = simplePeers [(nodeType, nodeId)]
            bracket
              (updatePeersBucket BucketSubscriptionListener (<> peers))
              (\added -> when added $ do
                void $ updatePeersBucket BucketSubscriptionListener (removePeer nodeId)
                logDebug $ sformat ("subscriptionListener: removed "%shown) nodeId)
              (\added -> when added $ do -- if not added, close the conversation
                  logDebug $ sformat ("subscriptionListener: added "%shown) nodeId
                  fix $ \loop -> recvLimited conv >>= \case
                      Just MsgSubscribeKeepAlive -> do
                          logDebug $ sformat
                              ("subscriptionListener: received keep-alive from "%shown)
                              nodeId
                          loop
                      msg -> logNotice $ expectedMsgFromGot MsgSubscribeKeepAlive
                                                            nodeId msg)
        msg -> logNotice $ expectedMsgFromGot MsgSubscribe nodeId msg
  where
    expectedMsgFromGot = sformat
            ("subscriptionListener: expected "%shown%" from "%shown%
             ", got "%shown%", closing the connection")

-- | Version of subscriptionListener for MsgSubscribe1.
subscriptionListener1
    :: forall pack m.
       (SubscriptionMode m)
    => OQ.OutboundQ pack NodeId Bucket
    -> NodeType
    -> (ListenerSpec m, OutSpecs)
subscriptionListener1 oq nodeType = listenerConv @Void oq $ \_ourVerInfo nodeId conv -> do
    mbMsg <- recvLimited conv
    whenJust mbMsg $ \MsgSubscribe1 -> do
      let peers = simplePeers [(nodeType, nodeId)]
      bracket
          (updatePeersBucket BucketSubscriptionListener (<> peers))
          (\added -> when added $ do
              void $ updatePeersBucket BucketSubscriptionListener (removePeer nodeId)
              logDebug $ sformat ("subscriptionListener1: removed "%shown) nodeId)
          (\added -> when added $ do -- if not added, close the conversation
              logDebug $ sformat ("subscriptionListener1: added "%shown) nodeId
              void $ recvLimited conv)

subscriptionListeners
    :: forall pack m.
       (SubscriptionMode m)
    => OQ.OutboundQ pack NodeId Bucket
    -> NodeType
    -> MkListeners m
subscriptionListeners oq nodeType = constantListeners
    [ subscriptionListener  oq nodeType
    , subscriptionListener1 oq nodeType
    ]

-- | Throw the standard subscription worker OutSpecs onto a given
-- implementation of a single subscription worker.
subscriptionWorker
    :: forall m. (SubscriptionMode m)
    => Worker m -> ([WorkerSpec m], OutSpecs)
subscriptionWorker theWorker = first (:[]) (worker subscriptionWorkerSpec theWorker)
  where
    subscriptionWorkerSpec :: OutSpecs
    subscriptionWorkerSpec = toOutSpecs
        [ convH (Proxy @MsgSubscribe)  (Proxy @Void)
        , convH (Proxy @MsgSubscribe1) (Proxy @Void)
        ]
