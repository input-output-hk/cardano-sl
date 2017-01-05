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
    , stopNode

    , MessageName

    , SendActions(sendTo, withConnectionTo)
    , ConversationActions(send, recv)
    , Worker
    , Listener(..)
    , ListenerAction(..)
    
    , LL.NodeId(..)
    , nodeId
    , nodeEndPointAddress

    ) where

import Control.Applicative (optional)
import Control.Monad (when, unless, void)
import Control.Monad.Fix (MonadFix)
import qualified Node.Internal as LL
import Node.Internal (ChannelIn(..), ChannelOut(..))
import Data.String (IsString)
import Data.Binary     as Bin
import Data.Binary.Put as Bin
import Data.Binary.Get as Bin
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Builder.Extra as BS
import Data.Maybe (fromMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Network.Transport.Abstract as NT
import System.Random (StdGen)
import Message.Message (MessageName)
import Mockable.Class
import Mockable.Concurrent
import Mockable.Channel
import Mockable.SharedAtomic
import Mockable.Exception
import GHC.Generics (Generic)
import Message.Message


data Node (m :: * -> *) = Node {
       nodeLL      :: LL.Node m,
       nodeWorkers :: [ThreadId m]
     }

nodeId :: Node m -> LL.NodeId
nodeId = LL.NodeId . NT.address . LL.nodeEndPoint . nodeLL

nodeEndPointAddress :: Node m -> NT.EndPointAddress
nodeEndPointAddress x = let LL.NodeId y = nodeId x in y

type Worker packing m = SendActions packing m -> m ()

data Listener packing m = Listener MessageName (ListenerAction packing m)

data ListenerAction  packing m where
  -- | A listener that handles a single isolated incoming message
  ListenerActionOneMsg
    :: ( Serializable packing msg )
    => (LL.NodeId -> SendActions packing m -> msg -> m ())
    -> ListenerAction packing m

  -- | A listener that handles an incoming bi-directional conversation.
  ListenerActionConversation
    :: ( Packable packing body, Unpackable packing rcv )
    => (LL.NodeId -> ConversationActions body rcv m -> m ())
    -> ListenerAction packing m

data SendActions packing m = SendActions {
       -- | Send a isolated (sessionless) message to a node
       sendTo :: forall body .
              ( Packable packing body )
              => LL.NodeId
              -> MessageName
              -> body
              -> m (),

       -- | Establish a bi-direction conversation session with a node.
       withConnectionTo
           :: forall body rcv.
            ( Packable packing body, Unpackable packing rcv )
           => LL.NodeId
           -> MessageName
           -> (ConversationActions body rcv m -> m ())
           -> m ()
     }

data ConversationActions body rcv m = ConversationActions {
       -- | Send a message within the context of this conversation
       send  :: body -> m (),

       -- | Receive a message within the context of this conversation.
       --   'Nothing' means end of input (peer ended conversation).
       recv  :: m (Maybe rcv)
     }

type ListenerIndex packing m = Map MessageName (ListenerAction packing m)

makeListenerIndex :: [Listener packing m] -> (ListenerIndex packing m, [MessageName])
makeListenerIndex = foldr combine (M.empty, [])
    where
    combine (Listener name action) (map, existing) =
        let (replaced, map') = M.insertLookupWithKey (\_ _ _ -> action) name action map
            overlapping = maybe [] (const [name]) replaced
        in  (map', overlapping ++ existing)

-- | Send actions for a given 'LL.Node'.
nodeSendActions
    :: forall m packing .
       ( Mockable Channel m, Mockable Throw m, Mockable Catch m
       , Mockable Bracket m, Mockable Fork m, Mockable SharedAtomic m
       , Packable packing MessageName )
    => LL.Node m
    -> packing
    -> SendActions packing m
nodeSendActions node packing = SendActions nodeSendTo nodeWithConnectionTo
    where

    nodeSendTo
        :: forall body .
           ( Packable packing body )
        => LL.NodeId
        -> MessageName
        -> body
        -> m ()
    nodeSendTo = \nodeId msgName body ->
        LL.withOutChannel node nodeId $ \channelOut ->
            mapM_ (LL.writeChannel channelOut . LBS.toChunks)
                [ packMsg packing msgName
                , packMsg packing body
                ]

    nodeWithConnectionTo
        :: forall body rcv .
           ( Packable packing body, Unpackable packing rcv )
        => LL.NodeId
        -> MessageName
        -> (ConversationActions body rcv m -> m ())
        -> m ()
    nodeWithConnectionTo = \nodeId msgName f ->
        LL.withInOutChannel node nodeId $ \inchan outchan -> do
            let cactions :: ConversationActions body rcv m
                cactions = nodeConversationActions node nodeId packing inchan outchan
                            msgName
            LL.writeChannel outchan . LBS.toChunks $
                packMsg packing msgName
            f cactions

-- | Conversation actions for a given peer and in/out channels.
nodeConversationActions
    :: forall packing snd rcv m .
       ( Mockable Throw m, Mockable Bracket m, Mockable Channel m
       , Packable packing snd
       , Packable packing MessageName
       , Unpackable packing rcv
       )
    => LL.Node m
    -> LL.NodeId
    -> packing
    -> ChannelIn m
    -> ChannelOut m
    -> MessageName
    -> ConversationActions snd rcv m
nodeConversationActions node nodeId packing inchan outchan msgName =
    ConversationActions nodeSend nodeRecv
    where

    nodeSend = \body ->
        LL.writeChannel outchan . LBS.toChunks $ packMsg packing body

    nodeRecv = do
        next <- recvNext inchan packing
        case next of
            End -> pure Nothing
            NoParse -> error "Unexpected end of conversation input"
            Input t -> pure (Just t)

-- | Spin up a node given a set of workers and listeners, using a given network
--   transport to drive it.
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
    -> [Worker packing m]
    -> [Listener packing m]
    -> m (Node m)
startNode endPoint prng packing workers listeners = do
    rec { node <- LL.startNode endPoint prng (handlerIn node sendActions) (handlerInOut node)
        ; let sendActions = nodeSendActions node packing
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
    listenerIndex :: ListenerIndex packing m
    (listenerIndex, conflictingNames) = makeListenerIndex listeners

    -- Handle incoming data from unidirectional connections: try to read the
    -- message name, use it to determine a listener, parse the body, then
    -- run the listener.
    handlerIn :: LL.Node m -> SendActions packing m -> LL.NodeId -> ChannelIn m -> m ()
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
    handlerInOut :: LL.Node m -> LL.NodeId -> ChannelIn m -> ChannelOut m -> m ()
    handlerInOut node peerId inchan outchan = do
        input <- recvNext inchan packing
        case input of
            End -> error "handlerInOut : unexpected end of input"
            NoParse -> error "handlerInOut : failed to parse message name"
            Input msgName -> do
                let listener = M.lookup msgName listenerIndex
                case listener of
                    Just (ListenerActionConversation action) ->
                        let cactions = nodeConversationActions node peerId packing
                                inchan outchan msgName
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
