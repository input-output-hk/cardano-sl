{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE NamedFieldPuns        #-}

-- | Demo for the outbound queue
module Network.Broadcast.OutboundQueue.Demo (relayDemo) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Function
import Data.Set (Set)
import Data.Text (Text)
import Formatting (sformat, (%), shown)
import System.Wlog
import qualified Data.Set as Set

import Network.Broadcast.OutboundQueue (OutboundQ)
import Network.Broadcast.OutboundQueue.Classification
import qualified Network.Broadcast.OutboundQueue as OutQ
import qualified Mockable as M

{-------------------------------------------------------------------------------
  Demo monads

  In order to show that it's possible, we use different monads for enqueueing
  and dequeueing.
-------------------------------------------------------------------------------}

newtype Dequeue a = Dequeue { unDequeue :: M.Production a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , M.Mockable M.Bracket
           , M.Mockable M.Catch
           , CanLog
           , HasLoggerName
           )

type instance M.ThreadId Dequeue = M.ThreadId M.Production
type instance M.Promise  Dequeue = M.Promise  M.Production

instance M.Mockable M.Async Dequeue where
  liftMockable = Dequeue . M.liftMockable . M.hoist' unDequeue
instance M.Mockable M.Fork  Dequeue where
  liftMockable = Dequeue . M.liftMockable . M.hoist' unDequeue

newtype Enqueue a = Enqueue { unEnqueue :: M.Production a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , CanLog
           , HasLoggerName
           )

runDequeue :: Dequeue a -> IO a
runDequeue = M.runProduction . unDequeue

runEnqueue :: Enqueue a -> IO a
runEnqueue = M.runProduction . unEnqueue

{-------------------------------------------------------------------------------
  Relay demo
-------------------------------------------------------------------------------}

relayDemo :: IO ()
relayDemo = do
    updateGlobalLogger "*production*" (setLevel Notice)

    let block :: Text -> [Node] -> Enqueue () -> Enqueue ()
        block label nodes act = do
          logNotice label
          act
          mapM_ (OutQ.flush . nodeOutQ) nodes
          liftIO $ threadDelay 500000

    -- Set up some test nodes

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

    runEnqueue $ do

      block "* Basic relay test: edge to core" [nodeR] $ do
        send Asynchronous (nodeEs !! 0) MsgTransaction (MsgId 0)

      block "* Basic relay test: code to edge" [nodeR] $ do
        send Asynchronous nodeC1 MsgAnnounceBlockHeader (MsgId 100)

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
          send Asynchronous nodeE MsgTransaction (MsgId n)

      block "* Priorities" [nodeR] $ do
        -- We schedule two transactions and a block header in quick succession.
        -- Although we enqueue the transactions before the block header, we
        -- should see in the output that the block headers are given priority.
        forM_ [300, 303 .. 309] $ \n -> do
          send Asynchronous nodeR MsgTransaction         (MsgId n)
          send Asynchronous nodeR MsgTransaction         (MsgId (n + 1))
          send Asynchronous nodeR MsgAnnounceBlockHeader (MsgId (n + 2))
          liftIO $ threadDelay 2500000

      block "* Latency masking (and sync API)" [nodeC2] $ do
        -- Core to core communication is allowed higher concurrency
        -- (We cannot send two blocks at a time though, because then MaxAhead
        -- would not be satisfiable).
        forM_ [400, 402 .. 408] $ \n -> do
          send Asynchronous nodeC3 MsgAnnounceBlockHeader (MsgId n)
          send Synchronous  nodeC3 MsgMPC                 (MsgId (n + 1))

      block "* Sending to specific nodes" nodeEs $ do
        -- This will send to the relay node
        sendTo Asynchronous nodeC1 [nodeC2, nodeR] MsgRequestBlock (MsgId 500)
        -- Edge nodes can never send to core nodes
        sendTo Asynchronous (nodeEs !! 0) [nodeC1] MsgRequestBlock (MsgId 501)

      logNotice "End of demo"

{-------------------------------------------------------------------------------
  Model of a node

  We model a node as a thread that relays any message it had not previously
  received.
-------------------------------------------------------------------------------}

data Node = Node {
      nodeType    :: NodeType
    , nodeId      :: NodeId
    , nodeMsgPool :: MVar (Set MsgId)
    , nodeOutQ    :: OutboundQ MsgObj_ NodeId
    }

-- | Create a new node, and spawn dequeue worker and forwarding listener
newNode :: NodeId_ -> NodeType -> CommsDelay -> IO Node
newNode nodeId_ nodeType commsDelay = do
    nodeOutQ     <- OutQ.new nodeId_
                             (OutQ.defaultEnqueuePolicy nodeType)
                             (OutQ.defaultDequeuePolicy nodeType)
                             (OutQ.defaultFailurePolicy nodeType)
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
    sendMsg :: OutQ.SendMsg Dequeue MsgObj_ NodeId
    sendMsg msg nodeId = liftIO $ msgSend msg nodeId

-- | Listener that forwards any new messages that arrive at the node
nodeForwardListener :: Node -> Enqueue ()
nodeForwardListener node = forever $ do
    msgData <- recvNodeId (nodeId node)
    added   <- addToMsgPool (nodeMsgPool node) msgData
    let msgObj = mkMsgObj msgData
    if not added then
      logDebug $ discarded msgObj
    else do
      logNotice $ received msgObj
      unless (msgType msgData == MsgRequestBlock) $ void $
        OutQ.enqueue (nodeOutQ node)
                     (msgType msgData)
                     msgObj
                     (OutQ.OriginForward (msgSender msgData))
                     mempty
  where
    received, discarded :: MsgObj -> Text
    received  = sformat (shown % ": received "  % formatMsg) (nodeId node)
    discarded = sformat (shown % ": discarded " % formatMsg) (nodeId node)

setPeers :: Node -> [Node] -> IO ()
setPeers peersOf = OutQ.subscribe (nodeOutQ peersOf) . simplePeers

simplePeers :: [Node] -> OutQ.Peers NodeId
simplePeers = OutQ.simplePeers . map (\n -> (nodeType n, nodeId n))

{-------------------------------------------------------------------------------
  Sending messages
-------------------------------------------------------------------------------}

data Sync = Synchronous | Asynchronous

-- | Send a message from the specified node
send :: Sync -> Node -> MsgType -> MsgId -> Enqueue ()
send sync from msgType msgId = do
    logNotice $ sformat (shown % ": send " % formatMsg) (nodeId from) msgObj
    True <- addToMsgPool (nodeMsgPool from) msgData
    enqueue (nodeOutQ from) msgType msgObj OutQ.OriginSender mempty
  where
    msgData = MsgData (nodeId from) msgType msgId
    msgObj  = mkMsgObj msgData
    enqueue = \oq mt conv origin peers -> case sync of
                Synchronous  -> void $ OutQ.enqueueSync oq mt conv origin peers
                Asynchronous -> void $ OutQ.enqueue oq mt conv origin peers

-- | Send a message to and from the specified nodes
sendTo :: Sync -> Node -> [Node] -> MsgType -> MsgId -> Enqueue ()
sendTo sync from to msgType msgId = do
    logNotice $ sformat (shown % ": send " % formatMsg) (nodeId from) msgObj
    True <- addToMsgPool (nodeMsgPool from) msgData
    enqueue (nodeOutQ from) msgType msgObj OutQ.OriginSender (simplePeers to)
  where
    msgData = MsgData (nodeId from) msgType msgId
    msgObj  = mkMsgObj msgData
    enqueue = \oq mt conv origin peers -> case sync of
                Synchronous  -> void $ OutQ.enqueueSyncTo oq mt conv origin peers
                Asynchronous -> void $ OutQ.enqueueTo oq mt conv origin peers

{-------------------------------------------------------------------------------
  Message pool
-------------------------------------------------------------------------------}

-- | Message pool allows us to detect whether an incoming message is new or not
type MsgPool = MVar (Set MsgId)

newMsgPool :: MonadIO m => m MsgPool
newMsgPool = liftIO $ newMVar Set.empty

-- | Add a message to the pool
--
-- Returns whether the message was new.
addToMsgPool :: MonadIO m => MsgPool -> MsgData -> m Bool
addToMsgPool pool MsgData{msgId} = liftIO $ modifyMVar pool $ \msgs ->
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
    , msgType   :: MsgType
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

sendNodeId :: MonadIO m => NodeId -> MsgData -> m ()
sendNodeId NodeId{..} = sendSyncVar nodeSyncVar

recvNodeId :: MonadIO m => NodeId -> m MsgData
recvNodeId NodeId{..} = recvSyncVar nodeSyncVar nodeDelay

{-------------------------------------------------------------------------------
  Model synchronous communication
-------------------------------------------------------------------------------}

data SyncVar a = SyncVar (MVar (a, MVar ()))

-- | Delay models slow communication networks
newtype CommsDelay = CommsDelay Int

newSyncVar :: MonadIO m => m (SyncVar a)
newSyncVar = liftIO $ SyncVar <$> newEmptyMVar

sendSyncVar :: MonadIO m => SyncVar a -> a -> m ()
sendSyncVar (SyncVar v) a = liftIO $ do
    ack <- newEmptyMVar
    putMVar v (a, ack)
    takeMVar ack

recvSyncVar :: MonadIO m => SyncVar a -> CommsDelay -> m a
recvSyncVar (SyncVar v) (CommsDelay delay) = liftIO $ do
    (a, ack) <- takeMVar v
    -- We run the acknowledgement in a separate thread, to model a node
    -- spawning a listener for each incoming request
    _tid <- forkIO $ do
      threadDelay delay
      putMVar ack ()
    return a
