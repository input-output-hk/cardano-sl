{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Demo for the outbound queue
module Network.Broadcast.OutboundQueue.Demo where


import           Control.Concurrent
import           Control.Exception (Exception, throwIO)
import           Control.Monad
import           Data.Function
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String (fromString)
import           Data.Text (Text)
import           Formatting (sformat, shown, (%))
import           System.Wlog

import           Pos.Util.Trace (wlogTrace)

import           Network.Broadcast.OutboundQueue (OutboundQ)
import qualified Network.Broadcast.OutboundQueue as OutQ
import           Network.Broadcast.OutboundQueue.Types hiding (simplePeers)

type Enqueue = IO

type Dequeue = IO

runDequeue :: Dequeue a -> IO a
runDequeue = id

runEnqueue :: Enqueue a -> IO a
runEnqueue = id

{-------------------------------------------------------------------------------
  Relay demo
-------------------------------------------------------------------------------}

relayDemo :: IO ()
relayDemo = do
    updateGlobalLogger "*production*" (setLevel noticePlus)

    let block :: Text -> [Node] -> Enqueue () -> Enqueue ()
        block label nodes act = do
          usingLoggerName (fromString "outboundqueue-production") $ logNotice label
          act
          mapM_ (OutQ.flush . nodeOutQ) nodes
          threadDelay 500000

    -- Set up some test nodes
    (nodeC1, nodeC2, nodeR, nodeEs, nodeC3) <- do
      nodeC1 <- newNode (C 1) NodeCore  (CommsDelay 0)
      nodeC2 <- newNode (C 2) NodeCore  (CommsDelay 0)
      nodeR  <- newNode (R 1) NodeRelay (CommsDelay 0)
      nodeEs <- forM [1 .. 9] $ \n -> newNode (E n) NodeEdge (CommsDelay 0)

      setPeers nodeR  (nodeC1 : nodeC2 : nodeEs)
      setPeers nodeC1 [nodeR]
      forM_ nodeEs $ \nodeE -> setPeers nodeE [nodeR]

      -- Two core nodes that communicate directly with each other
      -- (disjoint from the nodes we set up above)

      nodeC3 <- newNode (C 3) NodeCore (CommsDelay 0)
      nodeC4 <- newNode (C 4) NodeCore (CommsDelay 1000000)

      setPeers nodeC3 [nodeC4]
      return (nodeC1, nodeC2, nodeR, nodeEs, nodeC3)

    runEnqueue $ do

      block "* Basic relay test: edge to core" [nodeR] $ do
        send Asynchronous (nodeEs !! 0) (MsgTransaction OriginSender) (MsgId 0)

      block "* Basic relay test: code to edge" [nodeR] $ do
        send Asynchronous nodeC1 (MsgAnnounceBlockHeader OriginSender) (MsgId 100)

      -- In order to test rate limiting, we send a message from all of the edge
      -- nodes at once. These should then arrive at the (single) core node one
      -- at a time, rate limited by the intermediate relay node.
      --
      -- NOTE: Enqueueing a bunch of messages all at once from a single edge
      -- node is not a correct way to test this, because then the max ahead for
      -- edge nodes would stop those messages from being enqueued (unless we
      -- are lucky and they get enqueued in the gap between de dequeuer removing
      -- a message from the queue and it actually adding to the in-flight).
      block "* Rate limiting" [nodeR] $ do
        forM_ (zip nodeEs [200..209]) $ \(nodeE, n) ->
          send Asynchronous nodeE (MsgTransaction OriginSender) (MsgId n)

      block "* Priorities" [nodeR] $ do
        -- We schedule two transactions and a block header in quick succession.
        -- Although we enqueue the transactions before the block header, we
        -- should see in the output that the block headers are given priority.
        forM_ [300, 303 .. 309] $ \n -> do
          send Asynchronous nodeR (MsgTransaction OriginSender)         (MsgId n)
          send Asynchronous nodeR (MsgTransaction OriginSender)         (MsgId (n + 1))
          send Asynchronous nodeR (MsgAnnounceBlockHeader OriginSender) (MsgId (n + 2))
          threadDelay 2500000

      block "* Latency masking (and sync API)" [nodeC2] $ do
        -- Core to core communication is allowed higher concurrency
        -- (We cannot send two blocks at a time though, because then MaxAhead
        -- would not be satisfiable).
        forM_ [400, 402 .. 408] $ \n -> do
          send Asynchronous nodeC3 (MsgAnnounceBlockHeader OriginSender) (MsgId n)
          send Synchronous  nodeC3 (MsgMPC OriginSender)                 (MsgId (n + 1))

      block "* Sending to specific nodes" nodeEs $ do
        -- This will send to the relay node
        send Asynchronous nodeC1 (MsgRequestBlocks (Set.fromList (nodeId <$> [nodeC2, nodeR]))) (MsgId 500)
        -- Edge nodes can never send to core nodes
        send Asynchronous (nodeEs !! 0) (MsgRequestBlocks (Set.fromList (nodeId <$> [nodeC1]))) (MsgId 501)

      usingLoggerName (fromString "outboundqueue-demo") $ logNotice "End of demo"

{-------------------------------------------------------------------------------
  Model of a node

  We model a node as a thread that relays any message it had not previously
  received.
-------------------------------------------------------------------------------}

data Node = Node {
      nodeType    :: NodeType
    , nodeId      :: NodeId
    , nodeMsgPool :: MVar (Set MsgId)
    , nodeOutQ    :: OutboundQ MsgObj_ NodeId ()
    }

instance Eq Node where
    n1 == n2 = nodeId n1 == nodeId n2

-- | Create a new node, and spawn dequeue worker and forwarding listener
newNode :: NodeId_ -> NodeType -> CommsDelay -> IO Node
newNode nodeId_ nodeType commsDelay = do
    nodeOutQ     <- OutQ.new (wlogTrace (fromString (show nodeId_)))
                             demoEnqueuePolicy
                             demoDequeuePolicy
                             demoFailurePolicy
                             (const OutQ.BucketSizeUnlimited)
                             (OutQ.UnknownNodeType $ const NodeEdge)
    nodeId       <- NodeId nodeId_ commsDelay <$> newSyncVar
    nodeMsgPool  <- newMsgPool
    let node = Node{..}
    _worker   <- forkIO $ runDequeue $ nodeDequeueWorker node
    _listener <- forkIO $ runEnqueue $ nodeForwardListener node
    return node

-- | Worker that monitors the queue and sends all enqueued messages
nodeDequeueWorker :: Node -> Dequeue ()
nodeDequeueWorker node =
    OutQ.dequeueThread (nodeOutQ node) sendMsg
  where
    sendMsg :: OutQ.SendMsg MsgObj_ NodeId
    sendMsg msg nodeId = msgSend msg nodeId

-- | Listener that forwards any new messages that arrive at the node
nodeForwardListener :: Node -> Enqueue ()
nodeForwardListener node = forever $ do
    msgData <- recvNodeId (nodeId node)
    added   <- addToMsgPool (nodeMsgPool node) msgData
    let msgObj = mkMsgObj msgData
    if not added then
      usingLoggerName (fromString "outboundqueue-demo") $ logDebug $ discarded msgObj
    else do
      usingLoggerName (fromString "outboundqueue-demo") $ logNotice $ received msgObj
      let sender = msgSender msgData
          forwardMsgType = case msgType msgData of
            MsgAnnounceBlockHeader _ -> Just (MsgAnnounceBlockHeader (OriginForward sender))
            MsgRequestBlocks _       -> Nothing
            MsgRequestBlockHeaders _ -> Nothing
            MsgTransaction _         -> Just (MsgTransaction (OriginForward sender))
            MsgMPC _                 -> Just (MsgMPC (OriginForward sender))
      case forwardMsgType of
        Nothing -> return ()
        Just msgType' -> void $
          OutQ.enqueue (nodeOutQ node)
                       msgType'
                       msgObj
  where
    received, discarded :: MsgObj -> Text
    received  = sformat (shown % ": received "  % formatMsg) (nodeId node)
    discarded = sformat (shown % ": discarded " % formatMsg) (nodeId node)

-- | Set the peers of a node
setPeers :: Node -> [Node] -> IO ()
setPeers peersOf peers =
    void $ OutQ.updatePeersBucket (nodeOutQ peersOf) () (\_ -> simplePeers peers)

simplePeers :: [Node] -> OutQ.Peers NodeId
simplePeers = OutQ.simplePeers . map (\n -> (nodeType n, nodeId n))

{-------------------------------------------------------------------------------
  Sending messages
-------------------------------------------------------------------------------}

data Sync = Synchronous | Asynchronous

data SendFailed = SendFailedAddToPool
    deriving (Show)

instance Exception SendFailed

-- | Send a message from the specified node
send :: Sync -> Node -> MsgType NodeId -> MsgId -> Enqueue ()
send sync from msgType msgId = do
    usingLoggerName (fromString "outboundqueue-demo") $ logNotice $ sformat (shown % ": send " % formatMsg) (nodeId from) msgObj
    added <- addToMsgPool (nodeMsgPool from) msgData
    unless added $ throwIO SendFailedAddToPool
    enqueue (nodeOutQ from) msgType msgObj
  where
    msgData = MsgData (nodeId from) msgType msgId
    msgObj  = mkMsgObj msgData
    enqueue = \oq mt conv -> case sync of
                Synchronous  -> void $ OutQ.enqueueSync oq mt conv
                Asynchronous -> void $ OutQ.enqueue oq mt conv

{-------------------------------------------------------------------------------
  Message pool
-------------------------------------------------------------------------------}

-- | Message pool allows us to detect whether an incoming message is new or not
type MsgPool = MVar (Set MsgId)

newMsgPool :: IO MsgPool
newMsgPool = newMVar Set.empty

-- | Add a message to the pool
--
-- Returns whether the message was new.
addToMsgPool :: MsgPool -> MsgData -> IO Bool
addToMsgPool pool MsgData{msgId} = modifyMVar pool $ \msgs ->
    return $! if Set.member msgId msgs
                then (msgs, False)
                else (Set.insert msgId msgs, True)

{-------------------------------------------------------------------------------
  Messages
-------------------------------------------------------------------------------}

-- | Unique identifier for a message
newtype MsgId = MsgId Int
  deriving (Show, Eq, Ord)

data MsgData = MsgData {
      msgSender :: NodeId
    , msgType   :: MsgType NodeId
    , msgId     :: MsgId
    }
  deriving (Show)

-- | A message object is a message along with a callback to send it
data MsgObj_ a = MsgObj {
      msgData :: MsgData
    , msgSend :: NodeId -> IO a
    }

type MsgObj = MsgObj_ ()

instance FormatMsg MsgObj_ where
  formatMsg = (\k MsgObj{..} -> k msgData) <$> shown

mkMsgObj :: MsgData -> MsgObj
mkMsgObj msgData = MsgObj{..}
  where
    msgSend :: NodeId -> IO ()
    msgSend nid = sendNodeId nid msgData

{-------------------------------------------------------------------------------
  Node IDs
-------------------------------------------------------------------------------}

data NodeId_ = C Int | E Int | R Int
  deriving (Show, Eq, Ord)

-- | "Addressable" node IDs
--
-- Since we don't want to have to keep separate environments around
-- ("the network"), we just pair a 'NodeId_' with a 'SyncVar'.
data NodeId = NodeId {
      nodeId_     :: NodeId_
    , nodeDelay   :: CommsDelay
    , nodeSyncVar :: SyncVar MsgData
    }

instance Eq   NodeId where (==) = (==) `on` nodeId_
instance Ord  NodeId where (<=) = (<=) `on` nodeId_
instance Show NodeId where show = show .    nodeId_

sendNodeId :: NodeId -> MsgData -> IO ()
sendNodeId NodeId{..} = sendSyncVar nodeSyncVar

recvNodeId :: NodeId -> IO MsgData
recvNodeId NodeId{..} = recvSyncVar nodeSyncVar nodeDelay

{-------------------------------------------------------------------------------
  Model synchronous communication
-------------------------------------------------------------------------------}

data SyncVar a = SyncVar (MVar (a, MVar ()))

-- | Delay models slow communication networks
newtype CommsDelay = CommsDelay Int

newSyncVar :: IO (SyncVar a)
newSyncVar = SyncVar <$> newEmptyMVar

sendSyncVar :: SyncVar a -> a -> IO ()
sendSyncVar (SyncVar v) a = do
    ack <- newEmptyMVar
    putMVar v (a, ack)
    takeMVar ack

recvSyncVar :: SyncVar a -> CommsDelay -> IO a
recvSyncVar (SyncVar v) (CommsDelay delay) = do
    (a, ack) <- takeMVar v
    -- We run the acknowledgement in a separate thread, to model a node
    -- spawning a listener for each incoming request
    _tid <- forkIO $ do
      threadDelay delay
      putMVar ack ()
    return a

demoEnqueuePolicy :: OutQ.EnqueuePolicy nid
demoEnqueuePolicy _ = [OutQ.EnqueueOne {
    enqNodeTypes  = [NodeCore, NodeRelay, NodeEdge]
  , enqMaxAhead   = OutQ.MaxAhead 1
  , enqPrecedence = OutQ.PHigh
  }]

demoDequeuePolicy :: OutQ.DequeuePolicy
demoDequeuePolicy _ = OutQ.Dequeue {
    deqRateLimit   = OutQ.NoRateLimiting
  , deqMaxInFlight = OutQ.MaxInFlight 1
  }

demoFailurePolicy :: OutQ.FailurePolicy nid
demoFailurePolicy _ _ _ = OutQ.ReconsiderAfter 0
