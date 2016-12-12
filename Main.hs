{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import qualified NodeLowLevel as LL
import NodeLowLevel (NodeId)

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

type Worker = SendAction -> IO ()

type MessageName = BS.ByteString

data Listener = Listener MessageName ListenerAction

data ListenerAction where
  ListenerAction :: Binary msg => (ReplyAction -> msg -> IO ()) -> ListenerAction


data SendAction = SendAction {
       send :: forall body. Binary body =>
               NodeId -> MessageName -> body -> IO ()
     }

-- This is just a special case of 'SendAction' when we know who to send to
data ReplyAction = ReplyAction {
       reply :: forall body. Binary body =>
                MessageName -> body -> IO ()
     }


startNode :: NT.Transport -> [Worker] -> [Listener] -> IO Node
startNode transport workers listeners = do
    node <- LL.startNode transport handler
    let sendAction = SendAction { send = sendMsg node }
    tids <- sequence
              [ forkIO $ worker sendAction
              | worker <- workers ]
    return Node {
      nodeLL      = node,
      nodeWorkers = tids
    }
  where
    handler :: NodeId -> Chan (Maybe BS.ByteString) -> IO ()
    handler peer chan = return ()

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
    go (Bin.pushChunk prefix (Bin.runGetIncremental Bin.get))
  where
    go (Bin.Done trailing _ a) = return (a, trailing)
    go (Bin.Fail _trailing _ err) = fail "TODO"
    go (Bin.Partial continue) = do
      mx <- readChan chan
