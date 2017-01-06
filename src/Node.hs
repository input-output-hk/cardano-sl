{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}

module Node (

      Node
    , startNode
    , startNodeExt
    , stopNode

    , MessageName
    , Message (..)
    , messageName'

    , SendActions(sendTo, withConnectionTo, connStateTo, connStateStorage)
    , ConversationActions(send, recv, connState, convConnStateStorage)
    , Worker
    , Listener
    , ListenerAction(..)

    , LL.NodeId(..)
    , nodeId
    , nodeEndPointAddress

    ) where

import Control.Monad.Fix (MonadFix)
import qualified Node.Internal as LL
import Node.Internal (ChannelIn(..), ChannelOut(..))
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Proxy (Proxy (..))
import qualified Network.Transport.Abstract as NT
import System.Random (StdGen)
import Message.Message (MessageName)
import Mockable.Class
import Mockable.Concurrent
import Mockable.Channel
import Mockable.SharedAtomic
import Mockable.Exception
import Message.Message

data Node (m :: * -> *) = Node {
       nodeLL      :: LL.Node m,
       nodeWorkers :: [ThreadId m]
     }

nodeId :: Node m -> LL.NodeId
nodeId = LL.NodeId . NT.address . LL.nodeEndPoint . nodeLL

nodeEndPointAddress :: Node m -> NT.EndPointAddress
nodeEndPointAddress x = let LL.NodeId y = nodeId x in y

type Worker packing connState m = SendActions packing connState m -> m ()

-- TODO: rename all `ListenerAction` -> `Listener`?
type Listener = ListenerAction

data ListenerAction packing connState m where
  -- | A listener that handles a single isolated incoming message
  ListenerActionOneMsg
    :: ( Serializable packing msg, Message msg )
    => (LL.NodeId -> SendActions packing connState m -> msg -> m ())
    -> ListenerAction packing connState m

  -- | A listener that handles an incoming bi-directional conversation.
  ListenerActionConversation
    :: ( Packable packing snd, Unpackable packing rcv, Message rcv )
    => (LL.NodeId -> ConversationActions connState snd rcv m -> m ())
    -> ListenerAction packing connState m

-- | Gets message type basing on type of incoming messages
listenerMessageName :: Listener packing connState m -> MessageName
listenerMessageName (ListenerActionOneMsg f) =
    let msgName :: Message msg => (msg -> m ()) -> Proxy msg -> MessageName
        msgName _ = messageName
    in  msgName (f undefined undefined) Proxy
listenerMessageName (ListenerActionConversation f) =
    let msgName :: Message rcv
                => (ConversationActions connState snd rcv m -> m ())
                -> Proxy rcv
                -> MessageName
        msgName _ = messageName
    in  msgName (f undefined) Proxy

data SendActions packing connState m = SendActions {
       -- | Send a isolated (sessionless) message to a node
       sendTo :: forall msg .
              ( Packable packing msg, Message msg )
              => LL.NodeId
              -> msg
              -> m (),

       -- | Establish a bi-direction conversation session with a node.
       withConnectionTo
           :: forall snd rcv.
            ( Packable packing snd, Message snd, Unpackable packing rcv )
           => LL.NodeId
           -> (ConversationActions connState snd rcv m -> m ())
           -> m (),

        -- | Accesses state associated with connection to given node, creates if not exist
        connStateTo
            :: LL.NodeId
            -> m connState,

        -- | Returns shareable storage with states
        connStateStorage
            :: SharedAtomicT m (M.Map LL.NodeId connState)
     }

data ConversationActions connState body rcv m = ConversationActions {
       -- | Send a message within the context of this conversation
       send :: body -> m (),

       -- | Receive a message within the context of this conversation.
       --   'Nothing' means end of input (peer ended conversation).
       recv :: m (Maybe rcv),

       -- | Access state associated with this connection
       connState :: m connState,

       -- | Returns shareable storage with states
       convConnStateStorage :: SharedAtomicT m (M.Map LL.NodeId connState)
     }

type ListenerIndex packing connState m =
    Map MessageName (ListenerAction packing connState m)

-- | Stores information about current connection states.
data ConnectionsStates m s = ConnectionsStates
    { -- | Map with states. When connection is opened, state is created on first access
      connectionStates    :: SharedAtomicT m (M.Map LL.NodeId s)
      -- | Way to create state for new connection
    , initConnectionState :: m s
    }

makeListenerIndex :: [Listener packing connState m]
                  -> (ListenerIndex packing connState m, [MessageName])
makeListenerIndex = foldr combine (M.empty, [])
    where
    combine action (map, existing) =
        let name = listenerMessageName action
            (replaced, map') = M.insertLookupWithKey (\_ _ _ -> action) name action map
            overlapping = maybe [] (const [name]) replaced
        in  (map', overlapping ++ existing)

-- | Send actions for a given 'LL.Node'.
nodeSendActions
    :: forall m packing connState .
       ( Mockable Channel m, Mockable Throw m, Mockable Catch m
       , Mockable Bracket m, Mockable Fork m, Mockable SharedAtomic m
       , Packable packing MessageName )
    => LL.Node m
    -> packing
    -> ConnectionsStates m connState
    -> SendActions packing connState m
nodeSendActions node packing connStates =
    SendActions nodeSendTo nodeWithConnectionTo nodeConnStateTo nodeConnStates
  where

    nodeSendTo
        :: forall msg .
           ( Packable packing msg, Message msg )
        => LL.NodeId
        -> msg
        -> m ()
    nodeSendTo = \nodeId msg ->
        LL.withOutChannel node nodeId $ \channelOut ->
            LL.writeChannel channelOut $ concatMap LBS.toChunks
                [ packMsg packing $ messageName' msg
                , packMsg packing msg
                ]

    nodeWithConnectionTo
        :: forall snd rcv .
           ( Packable packing snd, Message snd, Unpackable packing rcv )
        => LL.NodeId
        -> (ConversationActions connState snd rcv m -> m ())
        -> m ()
    nodeWithConnectionTo = \nodeId f ->
        LL.withInOutChannel node nodeId $ \inchan outchan -> do
            let msgName  = messageName (Proxy :: Proxy snd)
                cactions :: ConversationActions connState snd rcv m
                cactions = nodeConversationActions node nodeId packing inchan outchan
                            connStates
            LL.writeChannel outchan . LBS.toChunks $
                packMsg packing msgName
            f cactions

    nodeConnStateTo = getConnState connStates

    nodeConnStates = connectionStates connStates

-- | Conversation actions for a given peer and in/out channels.
nodeConversationActions
    :: forall packing connState snd rcv m .
       ( Mockable Throw m, Mockable Bracket m, Mockable Channel m, Mockable SharedAtomic m
       , Packable packing snd
       , Packable packing MessageName
       , Unpackable packing rcv
       )
    => LL.Node m
    -> LL.NodeId
    -> packing
    -> ChannelIn m
    -> ChannelOut m
    -> ConnectionsStates m connState
    -> ConversationActions connState snd rcv m
nodeConversationActions node nodeId packing inchan outchan connStates =
    ConversationActions nodeSend nodeRecv nodeConnState convConnStateStorage
    where

    nodeSend = \body ->
        LL.writeChannel outchan . LBS.toChunks $ packMsg packing body

    nodeRecv = do
        next <- recvNext inchan packing
        case next of
            End -> pure Nothing
            NoParse -> error "Unexpected end of conversation input"
            Input t -> pure (Just t)

    nodeConnState = getConnState connStates nodeId

    convConnStateStorage = connectionStates connStates

startNode
    :: forall packing m .
       ( Mockable Fork m, Mockable Throw m, Mockable Channel m
       , Mockable SharedAtomic m, Mockable Bracket m, Mockable Catch m
       , MonadFix m
       , Serializable packing MessageName
       )
    => NT.EndPoint m
    -> StdGen
    -> packing
    -> [Worker packing () m]
    -> [Listener packing () m]
    -> m (Node m)
startNode endPoint prng packing workers listeners =
    startNodeExt endPoint prng packing (return ()) Nothing workers listeners

-- | Spin up a node given a set of workers and listeners, using a given network
--   transport to drive it.
startNodeExt
    :: forall packing connState m .
       ( Mockable Fork m, Mockable Throw m, Mockable Channel m
       , Mockable SharedAtomic m, Mockable Bracket m, Mockable Catch m
       , MonadFix m
       , Serializable packing MessageName
       )
    => NT.EndPoint m
    -> StdGen
    -> packing
    -> m connState
    -> Maybe (SharedAtomicT m (M.Map LL.NodeId connState))
    -> [Worker packing connState m]
    -> [Listener packing connState m]
    -> m (Node m)
startNodeExt endPoint prng packing initConnState mStatesStorage workers listeners = do
    statesStorage <- maybe (newSharedAtomic M.empty) return mStatesStorage
    let connStates = ConnectionsStates statesStorage initConnState
    rec { node <- LL.startNode endPoint prng (handlerIn node sendActions) (handlerInOut node connStates)
        ; let sendActions = nodeSendActions node packing connStates
        }
    tids <- sequence
              [ fork $ worker sendActions
              | worker <- workers ]
    return Node {
      nodeLL      = node,
      nodeWorkers = tids
    }
  where
    -- Index the listeners by message name, for faster lookup.
    -- TODO: report conflicting names, or statically eliminate them using
    -- DataKinds and TypeFamilies.
    listenerIndex :: ListenerIndex packing connState m
    (listenerIndex, conflictingNames) = makeListenerIndex listeners

    -- Handle incoming data from unidirectional connections: try to read the
    -- message name, use it to determine a listener, parse the body, then
    -- run the listener.
    handlerIn :: LL.Node m
              -> SendActions packing connState m
              -> LL.NodeId
              -> ChannelIn m
              -> m ()
    handlerIn node sendActions peerId inchan = do
        input <- recvNext inchan packing
        case input of
            End -> error "handerIn : unexpected end of input"
            -- TBD recurse and continue handling even after a no parse?
            NoParse -> error "handlerIn : failed to parse message name"
            Input msgName -> do
                let listener = M.lookup msgName listenerIndex
                case listener of
                    Just (ListenerActionOneMsg action) -> do
                        input' <- recvNext inchan packing
                        case input' of
                            End -> error "handerIn : unexpected end of input"
                            NoParse -> error "handlerIn : failed to parse message body"
                            Input msgBody -> do
                                action peerId sendActions msgBody
                    -- If it's a conversation listener, then that's an error, no?
                    Just (ListenerActionConversation _) -> error ("handlerIn : wrong listener type. Expected unidirectional for " ++ show msgName)
                    Nothing -> error ("handlerIn : no listener for " ++ show msgName)

    -- Handle incoming data from a bidirectional connection: try to read the
    -- message name, then choose a listener and fork a thread to run it.
    handlerInOut :: LL.Node m
                 -> ConnectionsStates m connState
                 -> LL.NodeId
                 -> ChannelIn m
                 -> ChannelOut m
                 -> m ()
    handlerInOut node connStates peerId inchan outchan = do
        input <- recvNext inchan packing
        case input of
            End -> error "handlerInOut : unexpected end of input"
            NoParse -> error "handlerInOut : failed to parse message name"
            Input msgName -> do
                let listener = M.lookup msgName listenerIndex
                case listener of
                    Just (ListenerActionConversation action) ->
                        let cactions = nodeConversationActions node peerId packing
                                inchan outchan connStates
                        in  action peerId cactions
                    Just (ListenerActionOneMsg _) -> error ("handlerInOut : wrong listener type. Expected bidirectional for " ++ show msgName)
                    Nothing -> error ("handlerInOut : no listener for " ++ show msgName)

stopNode :: ( Mockable Fork m ) => Node m -> m ()
stopNode Node {..} = do
    LL.stopNode nodeLL
    -- Stop the workers
    mapM_ killThread nodeWorkers
    -- alternatively we could try stopping new incoming messages
    -- and wait for all handlers to finish

recvNext
    :: ( Mockable Channel m, Unpackable packing thing )
    => ChannelIn m
    -> packing
    -> m (Input thing)
recvNext (ChannelIn chan) packing = unpackMsg packing chan

-- | Gets connection state. Creates it if absent
getConnState :: ( Mockable SharedAtomic m )
             => ConnectionsStates m s
             -> LL.NodeId
             -> m s
getConnState ConnectionsStates{..} nodeId =
    modifySharedAtomic connectionStates $
        \s -> do
            let v = M.lookup nodeId s
            case v of
                Nothing -> do
                    v' <- initConnectionState
                    return (M.insert nodeId v' s, v')
                Just x  -> return (s, x)
