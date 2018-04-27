-- | Common definitions for peer discovery and subscription workers.


module Pos.Diffusion.Subscription.Common
    ( SubscriptionMode
    , SubscriptionTerminationReason (..)
    , subscribeTo
    , subscriptionListeners
    , subscriptionWorker
    ) where

import           Universum

import           Control.Exception.Safe (try)
import           Control.Concurrent.MVar (modifyMVar_)
import qualified Data.Map.Strict as Map
import qualified Data.List.NonEmpty as NE
import           Data.Time.Units (Millisecond, Second, convertUnit, fromMicroseconds)
import qualified Network.Broadcast.OutboundQueue as OQ
import           Network.Broadcast.OutboundQueue.Types (removePeer, simplePeers)

import           Formatting (sformat, shown, (%))
import           Node.Message.Class (Message)
import           System.Clock (Clock (Monotonic), TimeSpec, getTime, toNanoSecs)
import           System.Wlog (WithLogger, logDebug, logNotice)

import           Pos.Binary.Class (Bi)
import           Pos.Communication.Listener (listenerConv)
import           Pos.Communication.Protocol (Conversation (..), ConversationActions (..),
                                             ListenerSpec, MkListeners, MsgSubscribe (..),
                                             MsgSubscribe1 (..), NodeId, OutSpecs,
                                             SendActions, constantListeners,
                                             convH, toOutSpecs, withConnectionTo,
                                             recvLimited, mlMsgSubscribe, mlMsgSubscribe1)
import           Pos.Diffusion.Types (SubscriptionStatus (..))
import           Pos.Network.Types (Bucket (..), NodeType)
import           Pos.Util.Timer (Timer, startTimer, waitTimer, setTimerDuration)
import           Pos.Worker.Types (Worker, WorkerSpec, worker)

type SubscriptionMode m =
    ( MonadIO m
    , MonadCatch m
    , WithLogger m
    , MonadMask m
    , Message MsgSubscribe
    , Message MsgSubscribe1
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
    => Timer
    -> TVar (Map NodeId SubscriptionStatus)
    -> MVar Millisecond -- ^ Subscription duration.
    -> SendActions m
    -> NodeId
    -> m SubscriptionTerminationReason
subscribeTo keepAliveTimer subStatus subDuration sendActions peer =
    do
        -- Change subscription status as we begin a new subscription
        alterPeerSubStatus (Just Subscribing)
        logNotice $ msgSubscribingTo peer
        subStarted <- liftIO $ getTime Monotonic
        -- 'try' is from safe-exceptions, so it won't catch asyncs.
        outcome <- try $ withConnectionTo sendActions peer $ \_peerData -> NE.fromList
            -- Sort conversations in descending order based on their version so that
            -- the highest available version of the conversation is picked.
            [ Conversation convMsgSubscribe
            , Conversation convMsgSubscribe1
            ]
        subEnded <- liftIO $ getTime Monotonic
        liftIO $ modifyMVar_ subDuration
            (\x -> return $! max x (timeSpecToMilliseconds $ subEnded - subStarted))
        let reason = either Exceptional (maybe Normal absurd) outcome
        logNotice $ msgSubscriptionTerminated peer reason
        return reason
    -- Change subscription state
    `finally` alterPeerSubStatus Nothing
  where
    convMsgSubscribe :: ConversationActions MsgSubscribe Void m -> m t
    convMsgSubscribe conv = do
        -- We are now subscribed, in rare cases when the connection will be
        -- dropped this will result in a missleading subscription state.
        alterPeerSubStatus (Just Subscribed)
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
            setTimerDuration keepAliveTimer $ convertUnit (20 :: Second)

    convMsgSubscribe1 :: ConversationActions MsgSubscribe1 Void m -> m (Maybe Void)
    convMsgSubscribe1 conv = do
        alterPeerSubStatus (Just Subscribed)
        send conv MsgSubscribe1
        recv conv 0 -- Other side will never send

    msgSubscribingTo :: NodeId -> Text
    msgSubscribingTo = sformat $ "subscriptionWorker: subscribing to "%shown

    msgSubscriptionTerminated :: NodeId -> SubscriptionTerminationReason -> Text
    msgSubscriptionTerminated = sformat $ "subscriptionWorker: lost connection to "%shown%" "%shown

    timeSpecToMilliseconds :: TimeSpec -> Millisecond
    timeSpecToMilliseconds = fromMicroseconds . div 1000 . toNanoSecs

    alterPeerSubStatus :: Maybe SubscriptionStatus -> m ()
    alterPeerSubStatus s = atomically $ do
        stats <- readTVar subStatus
        let !stats' = Map.alter fn peer stats
        writeTVar subStatus stats'
        where
            fn :: Maybe SubscriptionStatus -> Maybe SubscriptionStatus
            fn x = getOption (Option x <> Option s)

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
    recvLimited conv mlMsgSubscribe >>= \case
        Just MsgSubscribe -> do
            let peers = simplePeers [(nodeType, nodeId)]
            bracket
              (liftIO $ OQ.updatePeersBucket oq BucketSubscriptionListener (<> peers))
              (\added -> when added $ do
                void $ liftIO $ OQ.updatePeersBucket oq BucketSubscriptionListener (removePeer nodeId)
                logDebug $ sformat ("subscriptionListener: removed "%shown) nodeId)
              (\added -> when added $ do -- if not added, close the conversation
                  logDebug $ sformat ("subscriptionListener: added "%shown) nodeId
                  fix $ \loop -> recvLimited conv mlMsgSubscribe >>= \case
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
    mbMsg <- recvLimited conv mlMsgSubscribe1
    whenJust mbMsg $ \MsgSubscribe1 -> do
      let peers = simplePeers [(nodeType, nodeId)]
      bracket
          (liftIO $ OQ.updatePeersBucket oq BucketSubscriptionListener (<> peers))
          (\added -> when added $ do
              void $ liftIO $ OQ.updatePeersBucket oq BucketSubscriptionListener (removePeer nodeId)
              logDebug $ sformat ("subscriptionListener1: removed "%shown) nodeId)
          (\added -> when added $ do -- if not added, close the conversation
              logDebug $ sformat ("subscriptionListener1: added "%shown) nodeId
              void $ recvLimited conv mlMsgSubscribe1)

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
