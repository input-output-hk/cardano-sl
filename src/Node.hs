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
    , ForwardAction(..)
    , Worker
    , Listener(..)
    , ListenerAction(..)
    , PreListener

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
import Message.Util (fuseChannel)


data Node (m :: * -> *) = Node {
       nodeLL      :: LL.Node m,
       nodeWorkers :: [ThreadId m]
     }

nodeId :: Node m -> LL.NodeId
nodeId = LL.NodeId . NT.address . LL.nodeEndPoint . nodeLL

nodeEndPointAddress :: Node m -> NT.EndPointAddress
nodeEndPointAddress x = let LL.NodeId y = nodeId x in y

type Worker header packing m = SendActions header packing m -> m ()

data Listener header packing m = Listener MessageName (ListenerAction header packing m)

data ListenerAction header packing m where
  -- | A listener that handles a single isolated incoming message
  ListenerActionOneMsg
    :: ( Packable packing (ContentData msg), Unpackable packing (ContentData msg) )
    => (LL.NodeId -> SendActions header packing m -> msg -> m ())
    -> ListenerAction header packing m

  -- | A listener that handles an incoming bi-directional conversation.
  ListenerActionConversation
    :: ( Packable packing (ContentData header), Packable packing (ContentData body),
         Unpackable packing (ContentData rcv) )
    => (LL.NodeId -> ConversationActions header body rcv m -> m ())
    -> ListenerAction header packing m

type PreListener header packing m = header -> ForwardAction header packing m -> m Bool

data ForwardAction header packing m = ForwardAction {
        forward :: ( Packable packing (ContentData header) )
                => LL.NodeId
                -> header
                -> m ()
    }

data SendActions header packing m = SendActions {
       -- | Send a isolated (sessionless) message to a node
       sendTo :: forall body .
              ( Packable packing (ContentData header),
                 Packable packing (ContentData body) )
              => LL.NodeId
              -> MessageName
              -> header
              -> body
              -> m (),

       -- | Establish a bi-direction conversation session with a node.
       withConnectionTo
           :: forall body rcv.
            ( Packable packing (ContentData header),
              Packable packing (ContentData body),
              Unpackable packing (ContentData rcv) )
           => LL.NodeId
           -> MessageName
           -> (ConversationActions header body rcv m -> m ())
           -> m ()
     }

data ConversationActions header body rcv m = ConversationActions {
       -- | Send a message within the context of this conversation
       send  :: header -> body -> m (),

       -- | Receive a message within the context of this conversation.
       --   'Nothing' means end of input (peer ended conversation).
       recv  :: m (Maybe rcv)
     }

type ListenerIndex header packing m = Map MessageName (ListenerAction header packing m)

makeListenerIndex :: [Listener header packing m] -> (ListenerIndex header packing m, [MessageName])
makeListenerIndex = foldr combine (M.empty, [])
    where
    combine (Listener name action) (map, existing) =
        let (replaced, map') = M.insertLookupWithKey (\_ _ _ -> action) name action map
            overlapping = maybe [] (const [name]) replaced
        in  (map', overlapping ++ existing)

-- | Send actions for a given 'LL.Node'.
nodeSendActions
    :: forall m packing header .
       ( Mockable Channel m, Mockable Throw m, Mockable Catch m
       , Mockable Bracket m, Mockable Fork m, Mockable SharedAtomic m
       , Packable packing (ContentData MessageName)
       , Packable packing (ContentData header)
       , Unpackable packing (ContentData header)
       , Packable packing (ContentData LBS.ByteString)
       , Unpackable packing (ContentData BS.ByteString) )
    => LL.Node m
    -> packing
    -> PreListener header packing m
    -> SendActions header packing m
nodeSendActions node packing prelistener = SendActions nodeSendTo nodeWithConnectionTo
    where

    nodeSendTo
        :: forall body .
           ( Packable packing (ContentData body) )
        => LL.NodeId
        -> MessageName
        -> header
        -> body
        -> m ()
    nodeSendTo = \nodeId msgName header body ->
        LL.withOutChannel node nodeId $ \channelOut ->
            mapM_ (LL.writeChannel channelOut . LBS.toChunks)
                [ packMsg packing (ContentData msgName)
                , packMsg packing (WithHeaderData header (ContentData body))
                ]

    nodeWithConnectionTo
        :: forall body rcv .
           ( Packable packing (ContentData body), Unpackable packing (ContentData rcv) )
        => LL.NodeId
        -> MessageName
        -> (ConversationActions header body rcv m -> m ())
        -> m ()
    nodeWithConnectionTo = \nodeId msgName f ->
        LL.withInOutChannel node nodeId $ \inchan outchan -> do
            let cactions :: ConversationActions header body rcv m
                cactions = nodeConversationActions node nodeId packing inchan outchan
                            msgName prelistener
            LL.writeChannel outchan . LBS.toChunks $
                packMsg packing (ContentData msgName)
            f cactions

-- | Conversation actions for a given peer and in/out channels.
nodeConversationActions
    :: forall header packing snd rcv m .
       ( Mockable Throw m, Mockable Bracket m, Mockable Channel m
       , Packable packing (ContentData header)
       , Packable packing (ContentData snd)
       , Packable packing (ContentData LBS.ByteString)
       , Packable packing (ContentData MessageName)
       , Unpackable packing (FullData header)
       , Unpackable packing (ContentData header)
       , Unpackable packing (ContentData rcv)
       , Unpackable packing (ContentData BS.ByteString)
       )
    => LL.Node m
    -> LL.NodeId
    -> packing
    -> ChannelIn m
    -> ChannelOut m
    -> MessageName
    -> PreListener header packing m
    -> ConversationActions header snd rcv m
nodeConversationActions node nodeId packing inchan outchan msgName prelistener =
    ConversationActions nodeSend nodeRecv
    where

    nodeSend = \header body ->
        LL.writeChannel outchan . LBS.toChunks $
            packMsg packing (WithHeaderData header (ContentData body))

    nodeRecv = do
        next <- recvNext inchan packing
        case next of
            End -> pure Nothing
            NoParse -> error "Unexpected end of conversation input"
            Input (FullData header rawBody bodyExtractor) -> do
                let factions = nodeForwardAction node packing msgName rawBody
                toProcess <- prelistener header factions
                if toProcess
                    then case extract bodyExtractor packing of
                        Input (ContentData msgBody) -> return $ Just msgBody
                        _                           -> error
                            "nodeConversationActions : failed to extract message body"
                    else nodeRecv

-- | Forward actions for a given 'LL.Node'.
nodeForwardAction
    :: forall m packing header .
       ( Mockable Throw m, Mockable Bracket m
       , Packable packing (ContentData header)
       , Packable packing (ContentData MessageName) )
    => LL.Node m
    -> packing
    -> MessageName
    -> RawData
    -> ForwardAction header packing m
nodeForwardAction node packing msgName msgRawBody = ForwardAction nodeForward
    where

    nodeForward = \nodeId header ->
        LL.withOutChannel node nodeId $ \channelOut ->
            mapM_ (LL.writeChannel channelOut . LBS.toChunks)
                [ packMsg packing (ContentData msgName)
                , packMsg packing (WithHeaderData header msgRawBody)
                ]

-- | Spin up a node given a set of workers and listeners, using a given network
--   transport to drive it.
startNode
    :: forall header packing m .
       ( Mockable Fork m, Mockable Throw m, Mockable Channel m
       , Mockable SharedAtomic m, Mockable Bracket m, Mockable Catch m
       , MonadFix m
       , Packable packing (ContentData header)
       , Unpackable packing (ContentData header)
       , Packable packing (ContentData MessageName)
       , Unpackable packing (ContentData MessageName)
       , Unpackable packing (ContentData header)
       , Packable packing (ContentData LBS.ByteString)
       , Unpackable packing (ContentData BS.ByteString)
       )
    => NT.EndPoint m
    -> StdGen
    -> packing
    -> [Worker header packing m]
    -> Maybe (PreListener header packing m)
    -> [Listener header packing m]
    -> m (Node m)
startNode endPoint prng packing workers prelistener listeners = do
    rec { node <- LL.startNode endPoint prng (handlerIn node sendActions) (handlerInOut node)
        ; let sendActions = nodeSendActions node packing actualPrelistener
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
    listenerIndex :: ListenerIndex header packing m
    (listenerIndex, conflictingNames) = makeListenerIndex listeners

    -- If prelistener is not specified, provide a one which does nothing
    -- Note: prelistener is ever supported only by one-message communication style,
    -- behaviour for conversation style is undefined for now.
    actualPrelistener = fromMaybe (\_ _ -> return True) prelistener

    -- Handle incoming data from unidirectional connections: try to read the
    -- message name, use it to determine a listener, parse the body, then
    -- run the listener.
    handlerIn :: LL.Node m -> SendActions header packing m -> LL.NodeId -> ChannelIn m -> m ()
    handlerIn node sendActions peerId inchan = do
        input <- recvNext inchan packing
        case input of
            End -> error "handerIn : unexpected end of input"
            -- TBD recurse and continue handling even after a no parse?
            NoParse -> error "handlerIn : failed to parse message name"
            Input (ContentData msgName) -> do
                let listener = M.lookup msgName listenerIndex
                input' <- recvNext inchan packing
                case input' of
                    End -> error "handerIn : unexpected end of input"
                    NoParse -> error "handlerIn : failed to parse message body"
                    Input (FullData header msgRawBody bodyExtractor) -> do
                        let factions = nodeForwardAction node packing msgName msgRawBody
                        case listener of
                            Just (ListenerActionOneMsg action) -> do
                                toProcess <- actualPrelistener header factions
                                when toProcess $ case extract bodyExtractor packing of
                                    Input (ContentData msgBody) ->
                                        action peerId sendActions msgBody
                                    _                           -> error
                                        "handlerIn : failed to extract message body"
                            -- If it's a conversation listener, then that's an error, no?
                            Just (ListenerActionConversation _) ->
                                error ("handlerIn : wrong listener type. Expected unidirectional for " ++ show msgName)
                            Nothing ->
                                -- TODO: log that no listener matched
                                void $ actualPrelistener header factions

    -- Handle incoming data from a bidirectional connection: try to read the
    -- message name, then choose a listener and fork a thread to run it.
    handlerInOut :: LL.Node m -> LL.NodeId -> ChannelIn m -> ChannelOut m -> m ()
    handlerInOut node peerId inchan outchan = do
        input <- recvNext inchan packing
        case input of
            End -> error "handlerInOut : unexpected end of input"
            NoParse -> error "handlerInOut : failed to parse message name"
            Input (ContentData msgName) -> do
                let listener = M.lookup msgName listenerIndex
                case listener of
                    Just (ListenerActionConversation action) ->
                        let cactions = nodeConversationActions node peerId packing
                                inchan outchan msgName actualPrelistener
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

-- | Receive input from a ChannelIn.
--   If the channel's first element is 'Nothing' then it's the end of
--   input and you'll get 'End', otherwise we try to parse the 'thing'.
--   Unconsumed input is pushed back into the channel so that subsequent
--   'recvNext's will use it.
recvNext
    :: ( Mockable Channel m, Unpackable packing thing )
    => ChannelIn m
    -> packing
    -> m (Input thing)
recvNext (ChannelIn chan) packing = fuseChannel chan (unpackMsg packing)

-- FIXME
-- Serializing to "" is a problem. (), for instance, can't be used as data
-- over the wire.
-- It serializes to "". If we demand that the "" appear as a separate piece
-- of the channel then we're fine with the current implementation of recvNext.
-- If not, then even if a Nothing is pulled from the channel, we may still
-- parse a ().
