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

module Node where

import qualified Node.Internal as LL
import Node.Internal (NodeId, ChannelIn, ChannelOut)
import Data.Binary     as Bin
import Data.Binary.Put as Bin
import Data.Binary.Get as Bin
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Builder.Extra as BS
import qualified Network.Transport.Abstract as NT
import System.Random (RandomGen)
import Mockable.Class
import Mockable.Concurrent
import Mockable.Channel
import Mockable.SharedAtomic
import Mockable.Exception

data Node (m :: * -> *) = Node {
       nodeLL      :: LL.Node m,
       nodeWorkers :: [ThreadId m]
     }

type Worker m = SendActions m -> m ()

type MessageName = BS.ByteString

data Listener m = Listener MessageName (ListenerAction m)

data ListenerAction m where
  -- | A listener that handles a single isolated incoming message
  ListenerActionOneMsg
    :: Binary msg
    => (NodeId -> SendActions m -> msg -> m ())
    -> ListenerAction m

  -- | A listener that handles an incoming bi-directional conversation.
  ListenerActionConversation
    :: (NodeId -> ConversationActions m -> m ())
    -> ListenerAction m

data SendActions m = SendActions {
       -- | Send a isolated (sessionless) message to a node
       sendTo :: forall body. Binary body => NodeId -> MessageName -> body -> m (),

       -- | Establish a bi-direction conversation session with a node
       connect :: NodeId -> m (ConversationActions m)
     }

data ConversationActions m = ConversationActions {
       -- | Send a message within the context of this conversation
       send  :: forall msg. Binary msg => msg -> m (),

       -- | Receive a message within the context of this conversation
       recv  :: forall msg. Binary msg => m msg,

       -- | Close the outbound side of this conversation
       close :: m ()  -- TODO: needed? if so only for early close. Should by automatic.
     }

startNode
    :: forall m g .
       ( Mockable Fork m, Mockable RunInUnboundThread m, Mockable Throw m
       , Mockable Channel m, Mockable SharedAtomic m, RandomGen g )
    => NT.Transport m
    -> g
    -> [Worker m]
    -> [Listener m]
    -> m (Node m)
startNode transport prng workers listeners = do
    node <- LL.startNode transport prng handlerIn handlerInOut
    let sendAction = SendActions { sendTo = sendMsg node, connect = undefined }
    tids <- sequence
              [ fork $ worker sendAction
              | worker <- workers ]
    return Node {
      nodeLL      = node,
      nodeWorkers = tids
    }
  where
    handlerIn :: NodeId -> ChannelIn m -> m ()
    handlerIn peer chan = return ()
    --TODO: fill in the dispatcher impl:
    --      it needs to decode the MessageName
    --      then lookup the listener(s)
    --      then decode the body and fork the listener with the decoded message

    handlerInOut :: NodeId -> ChannelIn m -> ChannelOut m -> m ()
    handlerInOut peer inchan outchan = return ()
    --TODO: fill in the dispatcher impl:

stopNode :: ( Mockable Fork m ) => Node m -> m ()
stopNode Node {..} = do
    LL.stopNode nodeLL

    -- Stop the workers
    mapM_ killThread nodeWorkers
    -- alternatively we could try stopping new incoming messages
    -- and wait for all handlers to finish


sendMsg
    :: ( Monad m, Binary body )
    => LL.Node m
    -> NodeId
    -> MessageName
    -> body
    -> m ()
sendMsg node nodeid name body =
    LL.sendMsg node nodeid (serialiseMsg name body)

serialiseMsg :: Binary body => MessageName -> body -> LBS.ByteString
serialiseMsg name body =
    serialise $ do
      Bin.put name
      Bin.put body
  where
    serialise = BS.toLazyByteStringWith
                 (BS.untrimmedStrategy 256 4096)
                 LBS.empty
              . Bin.execPut

recvMsg
    :: ( Mockable Channel m, Binary msg )
    => ChannelT m (Maybe BS.ByteString)  -- source
    -> BS.ByteString                     -- prefix
    -> m (msg, BS.ByteString)            -- trailing
recvMsg chan prefix =
    go (Bin.pushChunk (Bin.runGetIncremental Bin.get) prefix)
  where
    go (Bin.Done trailing _ a) = return (a, trailing)
    go (Bin.Fail _trailing _ err) = fail "TODO"
    go (Bin.Partial continue) = do
      mx <- readChannel chan
      go (continue mx)
