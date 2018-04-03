{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTSyntax                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}

module Node (

      Node(..)
    , LL.NodeId(..)
    , LL.NodeEnvironment(..)
    , LL.defaultNodeEnvironment
    , LL.ReceiveDelay
    , LL.noReceiveDelay
    , LL.constantReceiveDelay
    , nodeEndPointAddress
    , NodeAction(..)
    , node

    , LL.NodeEndPoint(..)
    , simpleNodeEndPoint
    , manualNodeEndPoint

    , LL.NodeState(..)

    , MessageCode
    , Message (..)

    , Converse(..)
    , Conversation(..)
    , ConversationActions(send, recv)
    , converseWith

    , Listener (..)
    , ListenerAction

    , hoistListenerAction
    , hoistListener
    , hoistConversationActions

    , LL.Statistics(..)
    , LL.PeerStatistics(..)

    , LL.Timeout(..)

    ) where

import           Control.Exception.Safe (Exception (..), MonadMask, MonadThrow, SomeException,
                                         catch, onException, throwM)
import           Control.Monad (unless, when)
import           Control.Monad.Fix (MonadFix)
import qualified Data.ByteString as BS
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Proxy (Proxy (..))
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import           Data.Word (Word32)
import           Formatting (sformat, shown, (%))
import qualified Mockable.Channel as Channel
import           Mockable.Class
import           Mockable.Concurrent
import           Mockable.CurrentTime
import qualified Mockable.Metrics as Metrics
import           Mockable.SharedAtomic
import           Mockable.SharedExclusive
import qualified Network.Transport.Abstract as NT
import           Node.Conversation
import           Node.Internal (ChannelIn, ChannelOut)
import qualified Node.Internal as LL
import           Node.Message.Class (Message (..), MessageCode, Packing, Serializable (..), pack,
                                     unpack)
import           Node.Message.Decoder (ByteOffset, Decoder (..), DecoderStep (..), continueDecoding)
import           System.Random (StdGen)
import           System.Wlog (WithLogger, logDebug, logError, logInfo)

data Node m = Node {
      nodeId         :: LL.NodeId
    , nodeEndPoint   :: NT.EndPoint m
    , nodeStatistics :: m (LL.Statistics m)
    }

nodeEndPointAddress :: Node m -> NT.EndPointAddress
nodeEndPointAddress (Node addr _ _) = LL.nodeEndPointAddress addr

data Input t = Input t | End

data LimitExceeded = LimitExceeded
  deriving (Show, Typeable)

instance Exception LimitExceeded

-- | Custom exception thrown by recvNext
--
-- This carries the fields of the 'Fail' constructor of 'Decoder'
data NoParse = NoParse !BS.ByteString !ByteOffset !T.Text
  deriving (Show, Typeable)

instance Exception NoParse where
  displayException (NoParse trailing offset err) =
       "recvNext failed with " ++ T.unpack err
    ++ " (length trailing = " ++ show (BS.length trailing)
    ++ ", offset = " ++ show offset ++ ")"

-- | A ListenerAction with existential snd and rcv types and suitable
--   constraints on them.
data Listener packingType peerData m where
  Listener
    :: ( Serializable packingType snd, Serializable packingType rcv, Message rcv )
    => ListenerAction packingType peerData snd rcv m
    -> Listener packingType peerData m

-- | A listener that handles an incoming bi-directional conversation.
type ListenerAction packingType peerData snd rcv m =
       -- TODO do not take the peer data here, it's already in scope because
       -- the listeners are given as a function of the remote peer's
       -- peer data. This remains just because cardano-sl will need a big change
       -- to use it properly.
       peerData
    -> LL.NodeId
    -> ConversationActions snd rcv m
    -> m ()

hoistListenerAction
    :: ( )
    => (forall a. n a -> m a)
    -> (forall a. m a -> n a)
    -> ListenerAction packingType peerData snd rcv n
    -> ListenerAction packingType peerData snd rcv m
hoistListenerAction nat rnat f =
    \peerData nId convActions ->
        nat $ f peerData nId (hoistConversationActions rnat convActions)

hoistListener
    :: ( )
    => (forall a. n a -> m a)
    -> (forall a. m a -> n a)
    -> Listener packing peerData n
    -> Listener packing peerData m
hoistListener nat rnat (Listener la) = Listener $ hoistListenerAction nat rnat la

-- | Gets message type basing on type of incoming messages
listenerMessageCode :: Listener packing peerData m -> MessageCode
listenerMessageCode (Listener (
        _ :: peerData -> LL.NodeId -> ConversationActions snd rcv m -> m ()
    )) = messageCode (Proxy :: Proxy rcv)

type ListenerIndex packing peerData m =
    Map MessageCode (Listener packing peerData m)

makeListenerIndex :: [Listener packing peerData m]
                  -> (ListenerIndex packing peerData m, [MessageCode])
makeListenerIndex = foldr combine (M.empty, [])
    where
    combine action (dict, existing) =
        let name = listenerMessageCode action
            (replaced, dict') = M.insertLookupWithKey (\_ _ _ -> action) name action dict
            overlapping = maybe [] (const [name]) replaced
        in  (dict', overlapping ++ existing)

nodeConverse
    :: forall m packing peerData .
       ( Mockable Channel.Channel m
       , MonadMask m, Mockable SharedAtomic m, Mockable SharedExclusive m
       , Mockable LowLevelAsync m, Mockable Async m, Ord (ThreadId m)
       , Mockable CurrentTime m, Mockable Metrics.Metrics m
       , Mockable Delay m
       , WithLogger m, MonadFix m
       , Serializable packing peerData
       , Serializable packing MessageCode )
    => LL.Node packing peerData m
    -> Packing packing m
    -> Converse packing peerData m
nodeConverse nodeUnit packing = Converse nodeConverse
  where

    mtu = LL.nodeMtu (LL.nodeEnvironment nodeUnit)

    nodeConverse
        :: forall t .
           LL.NodeId
        -> (peerData -> Conversation packing m t)
        -> m t
    nodeConverse = \nodeId k ->
        LL.withInOutChannel nodeUnit nodeId $ \peerData inchan outchan -> case k peerData of
            Conversation (converse :: ConversationActions snd rcv m -> m t) -> do
                let msgCode = messageCode (Proxy :: Proxy snd)
                    cactions :: ConversationActions snd rcv m
                    cactions = nodeConversationActions nodeUnit nodeId packing inchan outchan
                pack packing msgCode >>= LL.writeMany mtu outchan
                converse cactions


-- | Conversation actions for a given peer and in/out channels.
nodeConversationActions
    :: forall packing peerData snd rcv m .
       ( MonadThrow m
       , Mockable Channel.Channel m
       , Serializable packing snd
       , Serializable packing rcv
       )
    => LL.Node packing peerData m
    -> LL.NodeId
    -> Packing packing m
    -> ChannelIn m
    -> ChannelOut m
    -> ConversationActions snd rcv m
nodeConversationActions node _ packing inchan outchan =
    ConversationActions nodeSend nodeRecv nodeSendRaw
    where

    mtu = LL.nodeMtu (LL.nodeEnvironment node)

    nodeSend = \body ->
        pack packing body >>= nodeSendRaw

    nodeRecv :: Word32 -> m (Maybe rcv)
    nodeRecv limit = do
        next <- recvNext packing (fromIntegral limit :: Int) inchan
        case next of
            End     -> pure Nothing
            Input t -> pure (Just t)

    nodeSendRaw = LL.writeMany mtu outchan

data NodeAction packing peerData m t =
    NodeAction (peerData -> [Listener packing peerData m])
               (Converse packing peerData m -> m t)

simpleNodeEndPoint
    :: NT.Transport m
    -> m (LL.Statistics m)
    -> LL.NodeEndPoint m
simpleNodeEndPoint transport _ = LL.NodeEndPoint {
      newNodeEndPoint = NT.newEndPoint transport
    , closeNodeEndPoint = NT.closeEndPoint
    }

manualNodeEndPoint
    :: ( Applicative m )
    => NT.EndPoint m
    -> m (LL.Statistics m)
    -> LL.NodeEndPoint m
manualNodeEndPoint ep _ = LL.NodeEndPoint {
      newNodeEndPoint = pure $ Right ep
    , closeNodeEndPoint = NT.closeEndPoint
    }

-- | Spin up a node. You must give a function to create listeners given the
--   'NodeId', and an action to do given the 'NodeId' and sending actions.
--
--   The 'NodeAction' must be lazy in the components of the 'Node' passed to
--   it. Its 'NodeId', for instance, may be useful for the listeners, but is
--   not defined until after the node's end point is created, which cannot
--   happen until the listeners are defined--as soon as the end point is brought
--   up, traffic may come in and cause a listener to run, so they must be
--   defined first.
--
--   The node will stop and clean up once that action has completed. If at
--   this time there are any listeners running, they will be allowed to
--   finish.
node
    :: forall packing peerData m t .
       ( Mockable Channel.Channel m
       , Mockable SharedAtomic m, MonadMask m
       , Mockable LowLevelAsync m, Mockable Async m
       , Ord (ThreadId m), Show (ThreadId m)
       , Mockable SharedExclusive m
       , Mockable Delay m
       , Mockable CurrentTime m, Mockable Metrics.Metrics m
       , MonadFix m, Serializable packing MessageCode, WithLogger m
       , Serializable packing peerData
       )
    => (m (LL.Statistics m) -> LL.NodeEndPoint m)
    -> (m (LL.Statistics m) -> LL.ReceiveDelay m)
       -- ^ delay on receiving input events.
    -> (m (LL.Statistics m) -> LL.ReceiveDelay m)
       -- ^ delay on receiving new connections.
    -> StdGen
    -> Packing packing m
    -> peerData
    -> LL.NodeEnvironment m
    -> (Node m -> NodeAction packing peerData m t)
    -> m t
node mkEndPoint mkReceiveDelay mkConnectDelay prng packing peerData nodeEnv k = do
    rec { let nId = LL.nodeId llnode
              endPoint = LL.nodeEndPoint llnode
              nodeUnit = Node nId endPoint (LL.nodeStatistics llnode)
              NodeAction mkListeners (act :: Converse packing peerData m -> m t) = k nodeUnit
              -- Index the listeners by message name, for faster lookup.
              -- TODO: report conflicting names, or statically eliminate them using
              -- DataKinds and TypeFamilies.
              listenerIndices :: peerData -> ListenerIndex packing peerData m
              listenerIndices = fmap (fst . makeListenerIndex) mkListeners
              converse :: Converse packing peerData m
              converse = nodeConverse llnode packing
              unexceptional :: m t
              unexceptional = do
                  t <- act converse
                  logNormalShutdown
                  (LL.stopNode llnode `catch` logNodeException)
                  return t
        ; llnode <- LL.startNode
              packing
              peerData
              (mkEndPoint . LL.nodeStatistics)
              (mkReceiveDelay . LL.nodeStatistics)
              (mkConnectDelay . LL.nodeStatistics)
              prng
              nodeEnv
              (handlerInOut llnode listenerIndices)
        }
    unexceptional
        `catch` logException
        `onException` (LL.stopNode llnode `catch` logNodeException)
  where
    logNormalShutdown :: m ()
    logNormalShutdown =
        logInfo $ sformat ("node stopping normally")
    logException :: forall s . SomeException -> m s
    logException e = do
        logError $ sformat ("node stopped with exception " % shown) e
        throwM e
    logNodeException :: forall s . SomeException -> m s
    logNodeException e = do
        logError $ sformat ("exception while stopping node " % shown) e
        throwM e
    -- Handle incoming data from a bidirectional connection: try to read the
    -- message name, then choose a listener and fork a thread to run it.
    handlerInOut
        :: LL.Node packing peerData m
        -> (peerData -> ListenerIndex packing peerData m)
        -> peerData
        -> LL.NodeId
        -> ChannelIn m
        -> ChannelOut m
        -> m ()
    handlerInOut nodeUnit listenerIndices peerData peerId inchan outchan = do
        let listenerIndex = listenerIndices peerData
        -- Use maxBound to receive the MessageCode.
        -- The understanding is that the Serializable instance for it against
        -- the given packing type shouldn't accept arbitrarily-long input (it's
        -- a Word16, surely it serializes to (2 + c) bytes for some c).
        input <- recvNext packing maxBound inchan
        case input of
            End -> logDebug "handlerInOut : unexpected end of input"
            Input msgCode -> do
                let listener = M.lookup msgCode listenerIndex
                case listener of
                    Just (Listener action) ->
                        let cactions = nodeConversationActions nodeUnit peerId packing inchan outchan
                        in  action peerData peerId cactions
                    Nothing -> error ("handlerInOut : no listener for " ++ show msgCode)

-- | Try to receive and parse the next message, subject to a limit on the
--   number of bytes which will be read.
--
--   An empty ByteString will never be passed to a decoder.
recvNext
    :: forall packing m thing .
       ( Mockable Channel.Channel m
       , MonadThrow m
       , Serializable packing thing
       )
    => Packing packing m
    -> Int
    -> ChannelIn m
    -> m (Input thing)
recvNext packing limit (LL.ChannelIn channel) = readNonEmpty (return End) $ \bs -> do
    -- limit' is the number of bytes that 'go' is allowed to pull.
    -- It's assumed that reading from the channel will bring in at most
    -- some limited number of bytes, so 'go' may bring in at most this
    -- many more than the limit.
    let limit' = limit - BS.length bs
    decoderStep <- runDecoder (unpack packing)
    (trailing, outcome) <- continueDecoding decoderStep bs >>= go limit'
    unless (BS.null trailing) (Channel.unGetChannel channel (Just trailing))
    return outcome
  where

    readNonEmpty :: m t -> (BS.ByteString -> m t) -> m t
    readNonEmpty nothing just = do
        mbs <- Channel.readChannel channel
        case mbs of
            Nothing -> nothing
            Just bs -> if BS.null bs then readNonEmpty nothing just else just bs

    go !remaining decoderStep = case decoderStep of
        Fail trailing offset err -> throwM $ NoParse trailing offset err
        Done trailing _ thing -> return (trailing, Input thing)
        Partial next -> do
            when (remaining < 0) (throwM LimitExceeded)
            readNonEmpty (runDecoder (next Nothing) >>= go remaining) $ \bs ->
                let remaining' = remaining - BS.length bs
                in  runDecoder (next (Just bs)) >>= go remaining'
