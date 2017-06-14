{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
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
{-# LANGUAGE BangPatterns               #-}

module Node (

      Node(..)
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

    , LL.NodeState(..)

    , MessageName
    , Message (..)
    , messageName'

    , Conversation(..)
    , SendActions(withConnectionTo)
    , ConversationActions(send, recv)
    , Worker
    , Listener
    , ListenerAction(..)

    , hoistListenerAction
    , hoistSendActions
    , hoistConversationActions
    , LL.NodeId(..)

    , LL.Statistics(..)
    , LL.PeerStatistics(..)

    , LL.Timeout(..)

    ) where

import           Control.Exception          (SomeException, Exception)
import           Control.Monad              (unless, when)
import           Control.Monad.Fix          (MonadFix)
import qualified Data.ByteString            as BS
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as M
import           Data.Proxy                 (Proxy (..))
import           Data.Typeable              (Typeable)
import           Data.Word                  (Word32)
import           Formatting                 (sformat, shown, (%))
import qualified Mockable.Channel           as Channel
import           Mockable.Class
import           Mockable.Concurrent
import           Mockable.CurrentTime
import           Mockable.Exception
import qualified Mockable.Metrics           as Metrics
import           Mockable.SharedAtomic
import           Mockable.SharedExclusive
import qualified Network.Transport.Abstract as NT
import           Node.Internal              (ChannelIn, ChannelOut)
import qualified Node.Internal              as LL
import           Node.Message.Class         (Serializable (..), MessageName,
                                             Message (..), messageName')
import           Node.Message.Decoder       (Decoder (..), continueDecoding)
import           System.Random              (StdGen)
import           System.Wlog                (WithLogger, logDebug, logError, logInfo)

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

data NoParse = NoParse
  deriving (Show, Typeable)

instance Exception NoParse

type Worker packing peerData m = SendActions packing peerData m -> m ()

-- TODO: rename all `ListenerAction` -> `Listener`?
type Listener = ListenerAction

data ListenerAction packing peerData m where
  -- | A listener that handles an incoming bi-directional conversation.
  ListenerActionConversation
    :: ( Serializable packing snd, Serializable packing rcv, Message rcv )
    => (peerData -> LL.NodeId -> ConversationActions snd rcv m -> m ())
    -> ListenerAction packing peerData m

hoistListenerAction
    :: (forall a. n a -> m a)
    -> (forall a. m a -> n a)
    -> ListenerAction packing peerData n
    -> ListenerAction packing peerData m
hoistListenerAction nat rnat (ListenerActionConversation f) = ListenerActionConversation $
    \peerData nId convActions -> nat $ f peerData nId (hoistConversationActions rnat convActions)

-- | Gets message type basing on type of incoming messages
listenerMessageName :: Listener packing peerData m -> MessageName
listenerMessageName (ListenerActionConversation (
        _ :: peerData -> LL.NodeId -> ConversationActions snd rcv m -> m ()
    )) = messageName (Proxy :: Proxy rcv)

-- | Use ConversationActions on some Serializable, Message send type, with a
--   Serializable receive type.
data Conversation packingType m t where
    Conversation
        :: (Serializable packingType snd, Serializable packingType rcv, Message snd)
        => (ConversationActions snd rcv m -> m t)
        -> Conversation packingType m t

data SendActions packing peerData m = SendActions {
       withConnectionTo :: forall t . LL.NodeId -> (peerData -> Conversation packing m t) -> m t
     }

data ConversationActions body rcv m = ConversationActions {
       -- | Send a message within the context of this conversation
       send :: body -> m ()

       -- | Receive a message within the context of this conversation.
       --   'Nothing' means end of input (peer ended conversation).
       --   The 'Word32' parameter is a limit on how many bytes will be read
       --   in by this use of 'recv'. If the limit is exceeded, the
       --   'LimitExceeded' exception is thrown.
     , recv :: Word32 -> m (Maybe rcv)
     }

hoistConversationActions
    :: (forall a. n a -> m a)
    -> ConversationActions body rcv n
    -> ConversationActions body rcv m
hoistConversationActions nat ConversationActions {..} =
  ConversationActions send' recv'
      where
        send' = nat . send
        recv' = nat . recv

hoistSendActions
    :: forall packing peerData n m .
       (forall a. n a -> m a)
    -> (forall a. m a -> n a)
    -> SendActions packing peerData n
    -> SendActions packing peerData m
hoistSendActions nat rnat SendActions {..} = SendActions withConnectionTo'
  where
    withConnectionTo'
        :: forall t . LL.NodeId -> (peerData -> Conversation packing m t) -> m t
    withConnectionTo' nodeId k = nat $ withConnectionTo nodeId $ \peerData -> case k peerData of
        Conversation l -> Conversation $ \cactions -> rnat (l (hoistConversationActions nat cactions))

type ListenerIndex packing peerData m =
    Map MessageName (ListenerAction packing peerData m)

makeListenerIndex :: [Listener packing peerData m]
                  -> (ListenerIndex packing peerData m, [MessageName])
makeListenerIndex = foldr combine (M.empty, [])
    where
    combine action (dict, existing) =
        let name = listenerMessageName action
            (replaced, dict') = M.insertLookupWithKey (\_ _ _ -> action) name action dict
            overlapping = maybe [] (const [name]) replaced
        in  (dict', overlapping ++ existing)

-- | Send actions for a given 'LL.Node'.
nodeSendActions
    :: forall m packing peerData .
       ( Mockable Channel.Channel m, Mockable Throw m, Mockable Catch m
       , Mockable Bracket m, Mockable SharedAtomic m, Mockable SharedExclusive m
       , Mockable Async m, Ord (ThreadId m)
       , Mockable CurrentTime m, Mockable Metrics.Metrics m
       , Mockable Delay m
       , WithLogger m, MonadFix m
       , Serializable packing peerData
       , Serializable packing MessageName )
    => LL.Node packing peerData m
    -> packing
    -> SendActions packing peerData m
nodeSendActions nodeUnit packing =
    SendActions nodeWithConnectionTo
  where

    mtu = LL.nodeMtu (LL.nodeEnvironment nodeUnit)

    nodeWithConnectionTo
        :: forall t .
           LL.NodeId
        -> (peerData -> Conversation packing m t)
        -> m t
    nodeWithConnectionTo = \nodeId k ->
        LL.withInOutChannel nodeUnit nodeId $ \peerData inchan outchan -> case k peerData of
            Conversation (converse :: ConversationActions snd rcv m -> m t) -> do
                let msgName = messageName (Proxy :: Proxy snd)
                    cactions :: ConversationActions snd rcv m
                    cactions = nodeConversationActions nodeUnit nodeId packing inchan outchan
                LL.writeMany mtu outchan (packMsg packing msgName)
                converse cactions

-- | Conversation actions for a given peer and in/out channels.
nodeConversationActions
    :: forall packing peerData snd rcv m .
       ( Mockable Throw m, Mockable Channel.Channel m, Mockable SharedExclusive m
       , WithLogger m
       , Serializable packing snd
       , Serializable packing rcv
       )
    => LL.Node packing peerData m
    -> LL.NodeId
    -> packing
    -> ChannelIn m
    -> ChannelOut m
    -> ConversationActions snd rcv m
nodeConversationActions node _ packing inchan outchan =
    ConversationActions nodeSend nodeRecv
    where

    mtu = LL.nodeMtu (LL.nodeEnvironment node)

    nodeSend = \body -> do
        LL.writeMany mtu outchan (packMsg packing body)

    nodeRecv :: Word32 -> m (Maybe rcv)
    nodeRecv limit = do
        next <- recvNext packing (fromIntegral limit :: Int) inchan
        case next of
            End     -> pure Nothing
            Input t -> pure (Just t)

data NodeAction packing peerData m t =
    NodeAction (peerData -> [Listener packing peerData m])
               (SendActions packing peerData m -> m t)

simpleNodeEndPoint
    :: NT.Transport m
    -> m (LL.Statistics m)
    -> LL.NodeEndPoint m
simpleNodeEndPoint transport _ = LL.NodeEndPoint {
      newNodeEndPoint = NT.newEndPoint transport
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
       ( Mockable Fork m, Mockable Throw m, Mockable Channel.Channel m
       , Mockable SharedAtomic m, Mockable Bracket m, Mockable Catch m
       , Mockable Async m, Mockable Concurrently m
       , Ord (ThreadId m), Show (ThreadId m)
       , Mockable SharedExclusive m
       , Mockable Delay m
       , Mockable CurrentTime m, Mockable Metrics.Metrics m
       , MonadFix m, Serializable packing MessageName, WithLogger m
       , Serializable packing peerData
       )
    => (m (LL.Statistics m) -> LL.NodeEndPoint m)
    -> (m (LL.Statistics m) -> LL.ReceiveDelay m)
    -> (m (LL.Statistics m) -> LL.ReceiveDelay m)
    -> StdGen
    -> packing
    -> peerData
    -> LL.NodeEnvironment m
    -> (Node m -> NodeAction packing peerData m t)
    -> m t
node mkEndPoint mkReceiveDelay mkConnectDelay prng packing peerData nodeEnv k = do
    rec { let nId = LL.nodeId llnode
        ; let endPoint = LL.nodeEndPoint llnode
        ; let nodeUnit = Node nId endPoint (LL.nodeStatistics llnode)
        ; let NodeAction mkListeners act = k nodeUnit
          -- Index the listeners by message name, for faster lookup.
          -- TODO: report conflicting names, or statically eliminate them using
          -- DataKinds and TypeFamilies.
        ; let listenerIndices :: peerData -> ListenerIndex packing peerData m
              listenerIndices = fmap (fst . makeListenerIndex) mkListeners
        ; llnode <- LL.startNode
              packing
              peerData
              (mkEndPoint . LL.nodeStatistics)
              (mkReceiveDelay . LL.nodeStatistics)
              (mkConnectDelay . LL.nodeStatistics)
              prng
              nodeEnv
              (handlerInOut llnode listenerIndices)
        ; let sendActions = nodeSendActions llnode packing
        }
    let unexceptional = do
            t <- act sendActions
            logNormalShutdown
            (LL.stopNode llnode `catch` logNodeException)
            return t
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
        throw e
    logNodeException :: forall s . SomeException -> m s
    logNodeException e = do
        logError $ sformat ("exception while stopping node " % shown) e
        throw e
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
        input <- recvNext packing messageNameSizeLimit inchan
        case input of
            End -> logDebug "handlerInOut : unexpected end of input"
            Input msgName -> do
                let listener = M.lookup msgName listenerIndex
                case listener of
                    Just (ListenerActionConversation action) ->
                        let cactions = nodeConversationActions nodeUnit peerId packing inchan outchan
                        in  action peerData peerId cactions
                    Nothing -> error ("handlerInOut : no listener for " ++ show msgName)
    -- Arbitrary limit on the message size...
    -- TODO make it configurable I guess.
    messageNameSizeLimit :: Int
    messageNameSizeLimit = 256

-- | Try to receive and parse the next message, subject to a limit on the
--   number of bytes which will be read.
recvNext
    :: ( Mockable Channel.Channel m
       , Mockable Throw m
       , Serializable packing thing
       )
    => packing
    -> Int
    -> ChannelIn m
    -> m (Input thing)
recvNext packing limit (LL.ChannelIn channel) = do
    -- Check whether the channel is depleted and End if so. Otherwise, push
    -- the bytes into the type's decoder and try to parse it before reaching
    -- the byte limit.
    mbs <- Channel.readChannel channel
    case mbs of
        Nothing -> return End
        Just bs -> do
            -- limit' is the number of bytes that 'go' is allowed to pull.
            -- It's assumed that reading from the channel will bring in at most
            -- some limited number of bytes, so 'go' may bring in at most this
            -- many more than the limit.
            let limit' = limit - BS.length bs
            (trailing, outcome) <- go limit' (continueDecoding (unpackMsg packing) [bs])
            unless (BS.null trailing) (Channel.unGetChannel channel (Just trailing))
            return outcome
  where
    go remaining decoder = case decoder of
        -- TODO use the error message in the exception.
        Fail _ _ _ -> throw NoParse
        Done trailing _ thing -> return (trailing, Input thing)
        Partial next -> do
            when (remaining <= 0) (throw LimitExceeded)
            mbs <- Channel.readChannel channel
            case mbs of
                Nothing -> return (BS.empty, End)
                Just bs ->
                    let !remaining' = remaining - BS.length bs
                    in  go remaining' (next (Just bs))
