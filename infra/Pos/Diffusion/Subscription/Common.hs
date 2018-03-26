-- | Common definitions for peer discovery and subscription workers.

{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Diffusion.Subscription.Common
    ( SubscriptionMessageConstraints
    , SubscriptionTerminationReason (..)

    , subscriptionListeners
    , subscriptionListener
    , subscriptionListener1
    
    , networkSubscribeTo
    , networkSubscribeTo'

    , updatePeersBucket
    , updatePeersBucketSubscribe
    , updatePeersBucketUnsubscribe
    ) where

import           Universum hiding (mask, catch)

import           Control.Exception (mask, catch, throwIO)
import           Control.Concurrent.MVar (modifyMVar, modifyMVar_)
import qualified Data.Map.Strict as Map
import qualified Data.List.NonEmpty as NE
import           Data.Time.Units (Millisecond, fromMicroseconds)
import qualified Network.Broadcast.OutboundQueue as OQ
import           Network.Broadcast.OutboundQueue.Types (removePeer, simplePeers)

import           Formatting (sformat, shown, (%))
import           Node.Message.Class (Message)
import           System.Clock (Clock (Monotonic), TimeSpec, getTime, toNanoSecs)
import           System.Wlog (Severity (..))

import           Pos.Binary.Class (Bi)
import           Pos.Communication.Listener (listenerConv)
import           Pos.Communication.Protocol (Conversation (..), ConversationActions (..),
                                             ListenerSpec, MkListeners, MsgSubscribe (..),
                                             MsgSubscribe1 (..), NodeId, OutSpecs,
                                             SendActions, constantListeners,
                                             withConnectionTo, recvLimited,
                                             mlMsgSubscribe, mlMsgSubscribe1)
import           Pos.Diffusion.Types (SubscriptionStatus (..))
import           Pos.Network.Types (Bucket (..), NodeType)
import           Pos.Util.Trace (Trace, traceWith)


-- | While holding the MVar, update the outbound queue bucket with the new
-- set of peers.
--
-- The routes in this bucket will be of a particular form: each of the
-- 'AllOf' conjuncts will consists of a singleton 'Alts'. 
updatePeersBucket
    :: forall pack nodeId bucket .
       ( Ord nodeId )
    => (Maybe Int -> Maybe Int)
    -> OQ.OutboundQ pack nodeId bucket
    -> bucket
    -> MVar (Map (NodeType, nodeId) Int)
    -> (NodeType, nodeId)
    -> IO ()
updatePeersBucket alteration oq bucket peersVar (nodeType, nodeId) =
    modifyMVar peersVar $ \peers -> do
        let !peers' = Map.alter alteration (nodeType, nodeId) peers
        void $ OQ.updatePeersBucket oq bucket $ \_ ->
            let classifiedList :: [(NodeType, [nodeId])]
                classifiedList = (\(a, b) -> (a, [b])) <$> Map.keys peers'
            in  OQ.peersFromList mempty classifiedList
        pure (peers', ())

updatePeersBucketSubscribe
    :: ( Ord nodeId )
    => OQ.OutboundQ pack nodeId bucket
    -> bucket
    -> MVar (Map (NodeType, nodeId) Int)
    -> (NodeType, nodeId)
    -> IO ()
updatePeersBucketSubscribe = updatePeersBucket alterationAdd
  where
    alterationAdd :: Maybe Int -> Maybe Int
    alterationAdd Nothing = Just 1
    alterationAdd (Just n) = Just (n + 1)

updatePeersBucketUnsubscribe
    :: ( Ord nodeId )
    => OQ.OutboundQ pack nodeId bucket
    -> bucket
    -> MVar (Map (NodeType, nodeId) Int)
    -> (NodeType, nodeId)
    -> IO ()
updatePeersBucketUnsubscribe = updatePeersBucket alterationRemove
  where
    alterationRemove :: Maybe Int -> Maybe Int
    alterationRemove Nothing = Nothing
    alterationRemove (Just 1) = Nothing
    alterationRemove (Just n) = Just (n - 1)

-- | Instances required in order to do network subscription.
type SubscriptionMessageConstraints =
    ( Message MsgSubscribe
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

-- | Subscribe via the network protocol, using MsgSubscribe or MsgSubscribe1
-- depending on the peer's version.
networkSubscribeTo
    :: ( SubscriptionMessageConstraints )
    => (NodeId -> IO r)  -- ^ Run before attempting to subscribe
    -> (NodeId -> IO ()) -- ^ Run after subscription comes up
    -> (NodeId -> IO ()) -- ^ In case the keepalive version is run, keepalive is sent when
                         --   this returns.
    -> (NodeId -> SubscriptionTerminationReason -> r -> IO ()) -- ^ Run after subscription ends
    -> SendActions
    -> NodeId
    -> IO ()
networkSubscribeTo before middle keepalive after sendActions peer = mask $ \restore -> do
    r <- before peer
    let networkAction = withConnectionTo sendActions peer $ \_peerData -> NE.fromList
            -- Sort conversations in descending order based on their version so that
            -- the highest available version of the conversation is picked.
            [ Conversation (convMsgSubscribe (middle peer) (keepalive peer))
            , Conversation (convMsgSubscribe1 (middle peer))
            ]
    t <- restore networkAction `catch` \e -> do
        after peer (Exceptional e) r
        throwIO e
    after peer Normal r
    pure t

convMsgSubscribe
    :: IO ()
    -> IO ()
    -> ConversationActions MsgSubscribe Void
    -> IO t
convMsgSubscribe subscribed keepalive conv = do
    subscribed
    send conv MsgSubscribe
    forever $ do
        keepalive
        send conv MsgSubscribeKeepAlive

convMsgSubscribe1 :: IO () -> ConversationActions MsgSubscribe1 Void -> IO ()
convMsgSubscribe1 subscribed conv = do
    subscribed
    send conv MsgSubscribe1
    maybe () absurd <$> recv conv 0 -- Other side will never send

-- | Network subscription with some bells and whistles.
--   - Updates a 'TVar' with the subscription status for the given 'NodeId'.
--   - Tracks the duration in 'Milliseconds' within an 'MVar'.
--   - Keeps an outbound queue up-to-date.
networkSubscribeTo'
    :: ( SubscriptionMessageConstraints )
    => Trace IO (Severity, Text)
    -> OQ.OutboundQ pack NodeId bucket
    -> bucket
    -> NodeType
    -> MVar (Map (NodeType, NodeId) Int)
    -> (NodeId -> IO ()) -- ^ Keepalive timeout
    -> TVar (Map NodeId SubscriptionStatus) -- ^ Subscription status per node.
    -> MVar Millisecond -- ^ How long did the last subscription last?
    -> SendActions
    -> NodeId
    -> IO ()
networkSubscribeTo' logTrace oq bucket nodeType peersVar keepalive status duration sendActions =
    networkSubscribeTo before middle keepalive after sendActions

  where

    before peer = do
        updatePeersBucketSubscribe oq bucket peersVar (nodeType, peer)
        alterPeerSubStatus peer (Just Subscribing)
        traceWith logTrace (Notice, msgSubscribingTo peer)
        getTime Monotonic

    middle peer = alterPeerSubStatus peer (Just Subscribed)

    after peer reason subStarted = do
        updatePeersBucketUnsubscribe oq bucket peersVar (nodeType, peer)
        subEnded <- getTime Monotonic
        modifyMVar_ duration $ \x ->
            pure $! max x (timeSpecToMilliseconds $ subEnded - subStarted)
        traceWith logTrace (Notice, msgSubscriptionTerminated peer reason)
        alterPeerSubStatus peer Nothing

    msgSubscribingTo :: NodeId -> Text
    msgSubscribingTo = sformat ("subscriptionWorker: subscribing to "%shown)

    msgSubscriptionTerminated :: NodeId -> SubscriptionTerminationReason -> Text
    msgSubscriptionTerminated = sformat ("subscriptionWorker: lost connection to "%shown%" "%shown)

    timeSpecToMilliseconds :: TimeSpec -> Millisecond
    timeSpecToMilliseconds = fromMicroseconds . div 1000 . toNanoSecs

    alterPeerSubStatus :: NodeId -> Maybe SubscriptionStatus -> IO ()
    alterPeerSubStatus peer s = atomically $ do
        stats <- readTVar status
        let !stats' = Map.alter fn peer stats
        writeTVar status stats'
        where
            fn :: Maybe SubscriptionStatus -> Maybe SubscriptionStatus
            fn x = getOption (Option x <> Option s)

-- | A listener for subscriptions: add the subscriber to the set of known
-- peers, annotating it with a given NodeType. Remove that peer from the set
-- of known peers when the connection is dropped.
subscriptionListener
    :: forall pack.
       ( SubscriptionMessageConstraints )
    => Trace IO (Severity, Text)
    -> OQ.OutboundQ pack NodeId Bucket
    -> NodeType
    -> (ListenerSpec, OutSpecs)
subscriptionListener logTrace oq nodeType = listenerConv @Void logTrace oq $ \__ourVerInfo nodeId conv -> do
    recvLimited conv mlMsgSubscribe >>= \case
        Just MsgSubscribe -> do
            let peers = simplePeers [(nodeType, nodeId)]
            bracket
              (liftIO $ OQ.updatePeersBucket oq BucketSubscriptionListener (<> peers))
              (\added -> when added $ do
                void $ liftIO $ OQ.updatePeersBucket oq BucketSubscriptionListener (removePeer nodeId)
                traceWith logTrace (Debug, sformat ("subscriptionListener: removed "%shown) nodeId))
              (\added -> when added $ do -- if not added, close the conversation
                  traceWith logTrace (Debug, sformat ("subscriptionListener: added "%shown) nodeId)
                  fix $ \loop -> recvLimited conv mlMsgSubscribe >>= \case
                      Just MsgSubscribeKeepAlive -> do
                          traceWith logTrace (Debug, sformat
                              ("subscriptionListener: received keep-alive from "%shown)
                              nodeId)
                          loop
                      msg -> traceWith logTrace (Notice, expectedMsgFromGot MsgSubscribeKeepAlive nodeId msg))
        msg -> traceWith logTrace (Notice, expectedMsgFromGot MsgSubscribe nodeId msg)
  where
    expectedMsgFromGot = sformat
            ("subscriptionListener: expected "%shown%" from "%shown%
             ", got "%shown%", closing the connection")

-- | Version of subscriptionListener for MsgSubscribe1.
subscriptionListener1
    :: forall pack.
       ( SubscriptionMessageConstraints )
    => Trace IO (Severity, Text)
    -> OQ.OutboundQ pack NodeId Bucket
    -> NodeType
    -> (ListenerSpec, OutSpecs)
subscriptionListener1 logTrace oq nodeType = listenerConv @Void logTrace oq $ \_ourVerInfo nodeId conv -> do
    mbMsg <- recvLimited conv mlMsgSubscribe1
    whenJust mbMsg $ \MsgSubscribe1 -> do
      let peers = simplePeers [(nodeType, nodeId)]
      bracket
          (liftIO $ OQ.updatePeersBucket oq BucketSubscriptionListener (<> peers))
          (\added -> when added $ do
              void $ liftIO $ OQ.updatePeersBucket oq BucketSubscriptionListener (removePeer nodeId)
              traceWith logTrace (Debug, sformat ("subscriptionListener1: removed "%shown) nodeId))
          (\added -> when added $ do -- if not added, close the conversation
              traceWith logTrace (Debug, sformat ("subscriptionListener1: added "%shown) nodeId)
              void $ recvLimited conv mlMsgSubscribe1)

subscriptionListeners
    :: forall pack.
       ( SubscriptionMessageConstraints )
    => Trace IO (Severity, Text)
    -> OQ.OutboundQ pack NodeId Bucket
    -> NodeType
    -> MkListeners
subscriptionListeners logTrace oq nodeType = constantListeners
    [ subscriptionListener  logTrace oq nodeType
    , subscriptionListener1 logTrace oq nodeType
    ]
