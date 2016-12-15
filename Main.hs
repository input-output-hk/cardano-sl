{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import qualified NodeLowLevel as LL
import NodeLowLevel (NodeId, ChannelIn, ChannelOut)

import Data.Binary     as Bin
import Data.Binary.Put as Bin
import Data.Binary.Get as Bin
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Builder.Extra as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Typeable
import Control.Exception
import Control.Concurrent
import qualified Network.Transport as NT

main = do
  return ()


data Node = Node {
       nodeLL      :: LL.Node,
       nodeWorkers :: [ThreadId]
     }

type Worker = SendActions -> IO ()

type MessageName = BS.ByteString

data Listener = Listener MessageName ListenerAction

data ListenerAction where
  -- | A listener that handles a single isolated incoming message
  ListenerActionOneMsg
    :: Binary msg
    => (NodeId -> SendActions -> msg -> IO ())
    -> ListenerAction

  -- | A listener that handles an incoming bi-directional conversation.
  ListenerActionConversation
    :: (NodeId -> ConversationActions -> IO ())
    -> ListenerAction

data SendActions = SendActions {
       -- | Send a isolated (sessionless) message to a node
       sendTo :: forall body. Binary body => NodeId -> MessageName -> body -> IO (),

       -- | Establish a bi-direction conversation session with a node
       connect :: NodeId -> IO ConversationActions
     }

data ConversationActions = ConversationActions {
       -- | Send a message within the context of this conversation
       send  :: forall msg. Binary msg => msg -> IO (),

       -- | Receive a message within the context of this conversation
       recv  :: forall msg. Binary msg => IO msg,

       -- | Close the outbound side of this conversation
       close :: IO ()  -- TODO: needed? if so only for early close. Should by automatic.
     }


startNode :: NT.Transport -> [Worker] -> [Listener] -> IO Node
startNode transport workers listeners = do
    node <- LL.startNode transport handlerIn handlerInOut
    let sendAction = SendActions { sendTo = sendMsg node, connect = undefined }
    tids <- sequence
              [ forkIO $ worker sendAction
              | worker <- workers ]
    return Node {
      nodeLL      = node,
      nodeWorkers = tids
    }
  where
    handlerIn :: NodeId -> ChannelIn -> IO ()
    handlerIn peer chan = return ()
    --TODO: fill in the dispatcher impl:
    --      it needs to decode the MessageName
    --      then lookup the listener(s)
    --      then decode the body and fork the listener with the decoded message

    handlerInOut :: NodeId -> ChannelIn -> ChannelOut -> IO ()
    handlerInOut peer inchan outchan = return ()
    --TODO: fill in the dispatcher impl:

stopNode :: Node -> IO ()
stopNode Node {..} = do
    LL.stopNode nodeLL

    -- Stop the workers
    mapM_ killThread nodeWorkers
    -- alternatively we could try stopping new incoming messages
    -- and wait for all handlers to finish


sendMsg :: Binary body => LL.Node -> NodeId -> MessageName -> body -> IO ()
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

recvMsg :: Binary msg
         => Chan (Maybe BS.ByteString)  -- source
         -> BS.ByteString               -- prefix
         -> IO (msg, BS.ByteString)     -- trailing
recvMsg chan prefix =
    go (Bin.pushChunk (Bin.runGetIncremental Bin.get) prefix)
  where
    go (Bin.Done trailing _ a) = return (a, trailing)
    go (Bin.Fail _trailing _ err) = fail "TODO"
    go (Bin.Partial continue) = do
      mx <- readChan chan
      go (continue mx)

