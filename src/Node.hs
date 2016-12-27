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
import Control.Monad (when, unless)
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


data Node (m :: * -> *) = Node {
       nodeLL      :: LL.Node m,
       nodeWorkers :: [ThreadId m]
     }

nodeId :: Node m -> LL.NodeId
nodeId = LL.NodeId . NT.address . LL.nodeEndPoint . nodeLL

nodeEndPointAddress :: Node m -> NT.EndPointAddress
nodeEndPointAddress x = let LL.NodeId y = nodeId x in y

type Worker header m = SendActions header m -> m ()

data Listener header m = Listener MessageName (ListenerAction header m)

data ListenerAction header m where
  -- | A listener that handles a single isolated incoming message
  ListenerActionOneMsg
    :: Binary msg
    => (LL.NodeId -> SendActions header m -> msg -> m ())
    -> ListenerAction header m

  -- | A listener that handles an incoming bi-directional conversation.
  ListenerActionConversation
    :: ( Binary header, Binary body, Binary rcv )
    => (LL.NodeId -> ConversationActions header body rcv m -> m ())
    -> ListenerAction header m

type PreListener header m = header -> ForwardAction header m -> m Bool

data ForwardAction header m = ForwardAction {
        forward :: ( Binary header )
                => LL.NodeId
                -> header
                -> m ()
    }

data SendActions header m = SendActions {
       -- | Send a isolated (sessionless) message to a node
       sendTo :: forall body. ( Binary header, Binary body )
              => LL.NodeId
              -> MessageName
              -> header
              -> body
              -> m (),

       -- | Establish a bi-direction conversation session with a node.
       withConnectionTo
           :: forall body rcv.
              ( Binary header, Binary body, Binary rcv )
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

type ListenerIndex header m = Map MessageName (ListenerAction header m)

makeListenerIndex :: [Listener header m] -> (ListenerIndex header m, [MessageName])
makeListenerIndex = foldr combine (M.empty, [])
    where
    combine (Listener name action) (map, existing) =
        let (replaced, map') = M.insertLookupWithKey (\_ _ _ -> action) name action map
            overlapping = maybe [] (const [name]) replaced
        in  (map', overlapping ++ existing)

-- | Send actions for a given 'LL.Node'.
nodeSendActions
    :: forall m header .
       ( Mockable Channel m, Mockable Throw m, Mockable Catch m
       , Mockable Bracket m, Mockable Fork m, Mockable SharedAtomic m
       , Binary header )
    => LL.Node m
    -> PreListener header m
    -> SendActions header m
nodeSendActions node prelistener = SendActions nodeSendTo nodeWithConnectionTo
    where

    nodeSendTo
        :: forall body .
           ( Binary body )
        => LL.NodeId
        -> MessageName
        -> header
        -> body
        -> m ()
    nodeSendTo = \nodeId msgName header body ->
        LL.withOutChannel node nodeId $ \channelOut ->
            LL.writeChannel channelOut (LBS.toChunks (serialiseMsg msgName header body))

    nodeWithConnectionTo
        :: forall body rcv .
           ( Binary body, Binary rcv )
        => LL.NodeId
        -> MessageName
        -> (ConversationActions header body rcv m -> m ())
        -> m ()
    nodeWithConnectionTo = \nodeId msgName f ->
        LL.withInOutChannel node nodeId $ \inchan outchan -> do
            let cactions :: ConversationActions header body rcv m
                cactions = nodeConversationActions node nodeId inchan outchan msgName
                            prelistener
            LL.writeChannel outchan (LBS.toChunks (serialiseMsgName msgName))
            f cactions

-- | Conversation actions for a given peer and in/out channels.
nodeConversationActions
    :: forall header snd rcv m .
       ( Mockable Throw m, Mockable Bracket m
       , Binary header, Binary snd, Binary rcv, Mockable Channel m )
    => LL.Node m
    -> LL.NodeId
    -> ChannelIn m
    -> ChannelOut m
    -> MessageName
    -> PreListener header m
    -> ConversationActions header snd rcv m
nodeConversationActions node nodeId inchan outchan msgName prelistener =
    ConversationActions nodeSend nodeRecv
    where

    nodeSend = \header body ->
        LL.writeChannel outchan (LBS.toChunks (serialiseHeaderAndBody header body))

    nodeRecv = do
        next <- recvHeaderAndBody inchan
        case next of
            End -> pure Nothing
            NoParse -> error "Unexpected end of conversation input"
            Input (header, rawBody) -> do
                let factions = nodeForwardAction node msgName rawBody
                toProcess <- prelistener header factions
                if toProcess
                    then case runGet (optional get) rawBody of
                        Nothing      -> error
                            "nodeConversationActions : failed to extract message body"
                        Just msgBody -> return $ Just msgBody
                    else nodeRecv

-- | Forward actions for a given 'LL.Node'.
nodeForwardAction
    :: forall m header .
       ( Mockable Throw m, Mockable Bracket m
       , Binary header )
    => LL.Node m
    -> MessageName
    -> LBS.ByteString
    -> ForwardAction header m
nodeForwardAction node msgName msgRawBody = ForwardAction nodeForward
    where

    nodeForward = \nodeId header ->
        LL.withOutChannel node nodeId $ \channelOut ->
            LL.writeChannel channelOut (LBS.toChunks (
                serialiseRawMsg msgName header msgRawBody
            ))

-- | Spin up a node given a set of workers and listeners, using a given network
--   transport to drive it.
startNode
    :: forall header m .
       ( Mockable Fork m, Mockable Throw m, Mockable Channel m
       , Mockable SharedAtomic m, Mockable Bracket m, Mockable Catch m
       , MonadFix m, Binary header )
    => NT.EndPoint m
    -> StdGen
    -> [Worker header m]
    -> Maybe (PreListener header m)
    -> [Listener header m]
    -> m (Node m)
startNode endPoint prng workers prelistener listeners = do
    rec { node <- LL.startNode endPoint prng (handlerIn node sendActions) (handlerInOut node)
        ; let sendActions = nodeSendActions node actualPrelistener
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
    listenerIndex :: ListenerIndex header m
    (listenerIndex, conflictingNames) = makeListenerIndex listeners

    -- If prelistener is not specified, provide a one which does nothing
    actualPrelistener = fromMaybe (const $ const $ return True) prelistener

    -- Handle incoming data from unidirectional connections: try to read the
    -- message name, use it to determine a listener, parse the body, then
    -- run the listener.
    handlerIn :: LL.Node m -> SendActions header m -> LL.NodeId -> ChannelIn m -> m ()
    handlerIn node sendActions peerId inchan = do
        (input :: Input MessageName) <- recvNext inchan
        case input of
            End -> error "handerIn : unexpected end of input"
            -- TBD recurse and continue handling even after a no parse?
            NoParse -> error "handlerIn : failed to parse message name"
            Input msgName -> do
                let listener = M.lookup msgName listenerIndex
                case listener of
                    Just (ListenerActionOneMsg action) -> do
                        input' <- recvHeaderAndBody inchan
                        case input' of
                            End -> error "handerIn : unexpected end of input"
                            NoParse -> error "handlerIn : failed to parse message body"
                            Input (header, msgRawBody) -> do
                                let factions = nodeForwardAction node msgName msgRawBody
                                toProcess <- actualPrelistener header factions
                                when toProcess $
                                    case runGet (optional get) msgRawBody of
                                        Nothing      -> error $ "handlerIn : failed to "
                                                        ++ " extract message body"
                                        Just msgBody -> action peerId sendActions msgBody
                    -- If it's a conversation listener, then that's an error, no?
                    Just (ListenerActionConversation _) -> error ("handlerIn : wrong listener type. Expected unidirectional for " ++ show msgName)
                    Nothing -> error ("handlerIn : no listener for " ++ show msgName)

    -- Handle incoming data from a bidirectional connection: try to read the
    -- message name, then choose a listener and fork a thread to run it.
    handlerInOut :: LL.Node m -> LL.NodeId -> ChannelIn m -> ChannelOut m -> m ()
    handlerInOut node peerId inchan outchan = do
        (input :: Input MessageName) <- recvNext inchan
        case input of
            End -> error "handlerInOut : unexpected end of input"
            NoParse -> error "handlerInOut : failed to parse message name"
            Input msgName -> do
                let listener = M.lookup msgName listenerIndex
                case listener of
                    Just (ListenerActionConversation action) ->
                        let cactions = nodeConversationActions node peerId inchan outchan
                                        msgName actualPrelistener
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

serialise :: Bin.Put -> LBS.ByteString
serialise = BS.toLazyByteStringWith
                 (BS.untrimmedStrategy 256 4096)
                 LBS.empty
              . Bin.execPut

serialiseMsgName
    :: MessageName
    -> LBS.ByteString
serialiseMsgName = serialise . put

serialiseHeaderAndBody
    :: ( Binary header, Binary body )
    => header
    -> body
    -> LBS.ByteString
serialiseHeaderAndBody header body =
    serialise $ do
      Bin.put header
      Bin.put $ Bin.encode body

serialiseMsg
    :: ( Binary header, Binary body )
    => MessageName
    -> header
    -> body
    -> LBS.ByteString
serialiseMsg name header body =
    serialise $ do
      Bin.put name
      Bin.put header
      Bin.put $ Bin.encode body

serialiseRawMsg
    :: ( Binary header )
    => MessageName
    -> header
    -> LBS.ByteString
    -> LBS.ByteString
serialiseRawMsg name header rawBody =
    serialise $ do
      Bin.put name
      Bin.put header
      Bin.put rawBody


data Input t where
    End :: Input t
    NoParse :: Input t
    Input :: t -> Input t

recvHeaderAndBody
    :: forall header m . ( Mockable Channel m, Binary header )
    => ChannelIn m
    -> m (Input (header, LBS.ByteString))
recvHeaderAndBody channelIn = do
    headerInput <- recvNext channelIn
    forInput headerInput $ \header -> do
        rawBodyInput <- recvNext channelIn
        forInput rawBodyInput $ \rawBody ->
            return $ Input (header, rawBody)
  where
    forInput :: Input t -> (t -> m (Input a)) -> m (Input a)
    forInput End       _ = pure End
    forInput NoParse   _ = pure NoParse
    forInput (Input t) f = f t


-- | Receive input from a ChannelIn.
--   If the channel's first element is 'Nothing' then it's the end of
--   input and you'll get 'End', otherwise we try to parse the 'thing'.
--   Unconsumed input is pushed back into the channel so that subsequent
--   'recvNext's will use it.
recvNext
    :: ( Mockable Channel m, Binary thing )
    => ChannelIn m
    -> m (Input thing)
recvNext (ChannelIn chan) = do
    mx <- readChannel chan
    case mx of
        Nothing -> pure End
        Just bs -> do
            (part, trailing) <- recvPart chan bs
            unless (BS.null trailing) $
                unGetChannel chan (Just trailing)
            case part of
                Nothing -> pure NoParse
                Just t -> pure (Input t)

-- FIXME
-- Serializing to "" is a problem. (), for instance, can't be used as data
-- over the wire.
-- It serializes to "". If we demand that the "" appear as a separate piece
-- of the channel then we're fine with the current implementation of recvNext.
-- If not, then even if a Nothing is pulled from the channel, we may still
-- parse a ().

recvPart
    :: ( Mockable Channel m, Binary thing )
    => ChannelT m (Maybe BS.ByteString)    -- source
    -> BS.ByteString                       -- prefix
    -> m (Maybe thing, BS.ByteString)      -- trailing
recvPart chan prefix =
    go (Bin.pushChunk (Bin.runGetIncremental Bin.get) prefix)
  where
    go (Bin.Done trailing _ a) = return (Just a, trailing)
    go (Bin.Fail trailing _ _) = return (Nothing, trailing)
    go (Bin.Partial continue) = do
      mx <- readChannel chan
      go (continue mx)
