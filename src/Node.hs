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
    , nodeEndPointAddress
    , NodeAction(..)
    , node

    , MessageName
    , Message (..)
    , messageName'

    , SendActions(sendTo, withConnectionTo)
    , ConversationActions(send, recv)
    , Worker
    , Listener
    , ListenerAction(..)

    , hoistListenerAction
    , hoistSendActions
    , hoistConversationActions
    , LL.NodeId(..)

    , nodeStatistics
    , LL.Statistics(..)
    , LL.PeerStatistics(..)

    ) where

import           Control.Monad.Fix          (MonadFix)
import           Control.Exception          (SomeException)
import qualified Data.ByteString.Lazy       as LBS
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as M
import           Data.Proxy                 (Proxy (..))
import           Mockable.Class
import           Mockable.Concurrent
import           Mockable.Exception
import           Mockable.SharedAtomic
import           Mockable.SharedExclusive
import qualified Network.Transport.ConnectionBuffers as CB
import qualified Network.Transport.Abstract as NT
import           Node.Internal              (ChannelIn, ChannelOut)
import qualified Node.Internal              as LL
import           Node.Message
import           System.Random              (StdGen)
import           Formatting                 (sformat, shown, (%))
import           System.Wlog                (WithLogger, logError, logDebug)

data Node m = forall event . Node {
      nodeId         :: LL.NodeId
    , nodeEndPoint   :: NT.EndPoint m event
    , nodeStatistics :: m LL.Statistics
    }

nodeEndPointAddress :: Node m -> NT.EndPointAddress
nodeEndPointAddress (Node addr _ _) = LL.nodeEndPointAddress addr

data Input t = Input t | NoParse | End

type Worker packing m = SendActions packing m -> m ()

-- TODO: rename all `ListenerAction` -> `Listener`?
type Listener = ListenerAction

data ListenerAction packing m where
  -- | A listener that handles a single isolated incoming message
  ListenerActionOneMsg
    :: ( Serializable packing msg, Message msg )
    => (LL.NodeId -> SendActions packing m -> msg -> m ())
    -> ListenerAction packing m

  -- | A listener that handles an incoming bi-directional conversation.
  ListenerActionConversation
    :: ( Packable packing snd, Unpackable packing rcv, Message rcv )
    => (LL.NodeId -> ConversationActions snd rcv m -> m ())
    -> ListenerAction packing m

hoistListenerAction :: (forall a. n a -> m a) -> (forall a. m a -> n a) -> ListenerAction p n -> ListenerAction p m
hoistListenerAction nat rnat (ListenerActionOneMsg f) = ListenerActionOneMsg $
    \nId sendActions -> nat . f nId (hoistSendActions rnat nat sendActions)
hoistListenerAction nat rnat (ListenerActionConversation f) = ListenerActionConversation $
    \nId convActions -> nat $ f nId (hoistConversationActions rnat convActions)

-- | Gets message type basing on type of incoming messages
listenerMessageName :: Listener packing m -> MessageName
listenerMessageName (ListenerActionOneMsg (
        _ :: LL.NodeId -> SendActions packing m -> msg -> m ()
    )) = messageName (Proxy :: Proxy msg)

listenerMessageName (ListenerActionConversation (
        _ :: LL.NodeId -> ConversationActions snd rcv m -> m ()
    )) = messageName (Proxy :: Proxy rcv)

data SendActions packing m = SendActions {
       -- | Send a isolated (sessionless) message to a node
       sendTo :: forall msg .
              ( Packable packing msg, Message msg )
              => LL.NodeId
              -> msg
              -> m (),

       -- | Establish a bi-direction conversation session with a node.
       withConnectionTo
           :: forall snd rcv t .
            ( Packable packing snd, Message snd, Unpackable packing rcv )
           => LL.NodeId
           -> (ConversationActions snd rcv m -> m t)
           -> m t
     }

data ConversationActions body rcv m = ConversationActions {
       -- | Send a message within the context of this conversation
       send :: body -> m (),

       -- | Receive a message within the context of this conversation.
       --   'Nothing' means end of input (peer ended conversation).
       recv :: m (Maybe rcv)
     }

hoistConversationActions :: (forall a. n a -> m a) -> ConversationActions body rcv n -> ConversationActions body rcv m
hoistConversationActions nat ConversationActions {..} =
  ConversationActions send' recv'
      where
        send' = nat . send
        recv' = nat recv

hoistSendActions :: (forall a. n a -> m a) -> (forall a. m a -> n a) -> SendActions p n -> SendActions p m
hoistSendActions nat rnat SendActions {..} = SendActions sendTo' withConnectionTo'
  where
    sendTo' nodeId msg = nat $ sendTo nodeId msg
    withConnectionTo' nodeId convActionsH =
        nat $ withConnectionTo nodeId  $ \convActions -> rnat $ convActionsH $ hoistConversationActions nat convActions

type ListenerIndex packing m =
    Map MessageName (ListenerAction packing m)

makeListenerIndex :: [Listener packing m]
                  -> (ListenerIndex packing m, [MessageName])
makeListenerIndex = foldr combine (M.empty, [])
    where
    combine action (dict, existing) =
        let name = listenerMessageName action
            (replaced, dict') = M.insertLookupWithKey (\_ _ _ -> action) name action dict
            overlapping = maybe [] (const [name]) replaced
        in  (dict', overlapping ++ existing)

-- | Send actions for a given 'LL.Node'.
nodeSendActions
    :: forall m packing .
       ( Mockable CB.Buffer m, Mockable Throw m, Mockable Catch m
       , Mockable Bracket m, Mockable SharedAtomic m, Mockable SharedExclusive m
       , Mockable Async m, Ord (Promise m ())
       , WithLogger m, MonadFix m
       , Packable packing MessageName )
    => LL.Node m
    -> packing
    -> SendActions packing m
nodeSendActions nodeUnit packing =
    SendActions nodeSendTo nodeWithConnectionTo
  where

    nodeSendTo
        :: forall msg .
           ( Packable packing msg, Message msg )
        => LL.NodeId
        -> msg
        -> m ()
    nodeSendTo = \nodeId msg ->
        LL.withOutChannel nodeUnit nodeId $ \channelOut ->
            LL.writeChannel channelOut $ concatMap LBS.toChunks
                [ packMsg packing $ messageName' msg
                , packMsg packing msg
                ]

    nodeWithConnectionTo
        :: forall snd rcv t .
           ( Packable packing snd, Message snd, Unpackable packing rcv )
        => LL.NodeId
        -> (ConversationActions snd rcv m -> m t)
        -> m t
    nodeWithConnectionTo = \nodeId f ->
        LL.withInOutChannel nodeUnit nodeId $ \inchan outchan -> do
            let msgName  = messageName (Proxy :: Proxy snd)
                cactions :: ConversationActions snd rcv m
                cactions = nodeConversationActions nodeUnit nodeId packing inchan
                    outchan
            LL.writeChannel outchan . LBS.toChunks $
                packMsg packing msgName
            f cactions

-- | Conversation actions for a given peer and in/out channels.
nodeConversationActions
    :: forall packing snd rcv m .
       ( Mockable Throw m, Mockable CB.Buffer m, Mockable SharedExclusive m
       , WithLogger m
       , Packable packing snd
       , Unpackable packing rcv
       )
    => LL.Node m
    -> LL.NodeId
    -> packing
    -> ChannelIn m
    -> ChannelOut m
    -> ConversationActions snd rcv m
nodeConversationActions _ _ packing inchan outchan =
    ConversationActions nodeSend nodeRecv
    where

    nodeSend = \body -> do
        LL.writeChannel outchan . LBS.toChunks $ packMsg packing body

    nodeRecv = do
        next <- recvNext inchan packing
        case next of
            End     -> pure Nothing
            NoParse -> do
                logDebug "Unexpected end of conversation input"
                pure Nothing
            Input t -> pure (Just t)

data NodeAction packing m t = NodeAction [Listener packing m] (SendActions packing m -> m t)

-- | Spin up a node. You must give a function to create listeners given the
--   'NodeId', and an action to do given the 'NodeId' and sending actions.
--   The node will stop and clean up once that action has completed. If at
--   this time there are any listeners running, they will be allowed to
--   finished.
node
    :: forall packing m t .
       ( Mockable Fork m, Mockable Throw m, Mockable CB.Buffer m
       , Mockable SharedAtomic m, Mockable Bracket m, Mockable Catch m
       , Mockable Async m, Mockable Concurrently m, Ord (Promise m ())
       , Mockable SharedExclusive m
       , MonadFix m, Serializable packing MessageName, WithLogger m
       )
    => NT.Transport m
    -> StdGen
    -> packing
    -> (Node m -> m (NodeAction packing m t))
    -> m t
node transport prng packing k = do
    rec { llnode <- LL.startNode transport prng (handlerIn listenerIndex sendActions) (handlerInOut llnode listenerIndex)
        ; let nId = LL.nodeId llnode
        ; let endPoint = LL.nodeEndPoint llnode
        ; let nodeUnit = Node nId endPoint (LL.nodeStatistics llnode)
        ; NodeAction listeners act <- k nodeUnit
          -- Index the listeners by message name, for faster lookup.
          -- TODO: report conflicting names, or statically eliminate them using
          -- DataKinds and TypeFamilies.
        ; let listenerIndex :: ListenerIndex packing m
              (listenerIndex, _conflictingNames) = makeListenerIndex listeners
        ; let sendActions = nodeSendActions llnode packing
        }
    act sendActions `finally` LL.stopNode llnode `catch` logException
  where
    logException :: SomeException -> m t
    logException e = do
        logError (sformat ("node stopped with exception " % shown) e)
        throw e
    -- Handle incoming data from unidirectional connections: try to read the
    -- message name, use it to determine a listener, parse the body, then
    -- run the listener.
    handlerIn :: ListenerIndex packing m -> SendActions packing m -> LL.NodeId -> ChannelIn m -> m ()
    handlerIn listenerIndex sendActions peerId inchan = do
        input <- recvNext inchan packing
        case input of
            End -> logDebug "handerIn : unexpected end of input"
            -- TBD recurse and continue handling even after a no parse?
            NoParse -> logDebug "handlerIn : failed to parse message name"
            Input msgName -> do
                let listener = M.lookup msgName listenerIndex
                case listener of
                    Just (ListenerActionOneMsg action) -> do
                        input' <- recvNext inchan packing
                        case input' of
                            End -> logDebug "handerIn : unexpected end of input"
                            NoParse -> logDebug "handlerIn : failed to parse message body"
                            Input msgBody -> do
                                action peerId sendActions msgBody
                    -- If it's a conversation listener, then that's an error, no?
                    Just (ListenerActionConversation _) -> error ("handlerIn : wrong listener type. Expected unidirectional for " ++ show msgName)
                    Nothing -> error ("handlerIn : no listener for " ++ show msgName)

    -- Handle incoming data from a bidirectional connection: try to read the
    -- message name, then choose a listener and fork a thread to run it.
    handlerInOut :: LL.Node m
                 -> ListenerIndex packing m
                 -> LL.NodeId
                 -> ChannelIn m
                 -> ChannelOut m
                 -> m ()
    handlerInOut nodeUnit listenerIndex peerId inchan outchan = do
        input <- recvNext inchan packing
        case input of
            End -> logDebug "handlerInOut : unexpected end of input"
            NoParse -> logDebug "handlerInOut : failed to parse message name"
            Input msgName -> do
                let listener = M.lookup msgName listenerIndex
                case listener of
                    Just (ListenerActionConversation action) ->
                        let cactions = nodeConversationActions nodeUnit peerId packing
                                inchan outchan
                        in  action peerId cactions
                    Just (ListenerActionOneMsg _) -> error ("handlerInOut : wrong listener type. Expected bidirectional for " ++ show msgName)
                    Nothing -> error ("handlerInOut : no listener for " ++ show msgName)

recvNext
    :: ( Mockable CB.Buffer m, Mockable SharedExclusive m
       , Unpackable packing thing )
    => ChannelIn m
    -> packing
    -> m (Input thing)
recvNext channelIn packing = LL.readChannel packing End End NoParse Input channelIn
