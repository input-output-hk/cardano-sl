{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Demo for the outbound queue
module Network.Broadcast.OutboundQueue.Demo (relayDemo) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Function
import Data.Set (Set)
import Data.Text (Text)
import Formatting (sformat, (%), shown, later)
import System.Wlog
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Internal.Builder as T (fromText)

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

    nodeC1 <- newNode $ NodeCfg (C 1) NodeCore  (CommsDelay 0)
    nodeR  <- newNode $ NodeCfg (R 1) NodeRelay (CommsDelay 0)
    nodeEs <- forM [1 .. 9] $ \n ->
                newNode $ NodeCfg (E n) NodeEdge (CommsDelay 0)

    setPeers nodeR  (nodeC1 : nodeEs)
    setPeers nodeC1 [nodeR]
    forM_ nodeEs $ \nodeE -> setPeers nodeE [nodeR]

    -- Two core nodes that communicate directly with each other
    -- (disjoint from the nodes we set up above)

    nodeC2 <- newNode $ NodeCfg (C 2) NodeCore (CommsDelay 0)
    nodeC3 <- newNode $ NodeCfg (C 3) NodeCore (CommsDelay 1000000)

    setPeers nodeC2 [nodeC3]

    runEnqueue $ do

      block "* Basic relay test: edge to core" [nodeR] $ do
        send $ mkMsg (nodeEs !! 0) MsgTransaction 0

      block "* Basic relay test: code to edge" [nodeR] $ do
        send $ mkMsg nodeC1 MsgBlockHeader 100

      -- In order to test rate limiting, we sendSyncVar a message from all of the edge
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
          send $ mkMsg nodeE MsgTransaction n

      block "* Priorities" [nodeR] $ do
        -- We schedule two transactions and a block header in quick succession.
        -- Although we enqueue the transactions before the block header, we
        -- should see in the output that the block headers are given priority.
        forM_ [300, 303 .. 309] $ \n -> do
          send $ mkMsg nodeR MsgTransaction n
          send $ mkMsg nodeR MsgTransaction (n + 1)
          send $ mkMsg nodeR MsgBlockHeader (n + 2)
          liftIO $ threadDelay 2500000

      block "* Latency masking (and sync API)" [nodeC2] $ do
        -- Core to core communication is allowed higher concurrency
        -- (We cannot sendSyncVar two blocks at a time though, because then MaxAhead
        -- would not be satisfiable).
        forM_ [400, 402 .. 408] $ \n -> do
          send     $ mkMsg nodeC2 MsgBlockHeader n
          sendSync $ mkMsg nodeC2 MsgMPC        (n + 1)

      logNotice "End of demo"

{-------------------------------------------------------------------------------
  Outbound queue used for the demo
-------------------------------------------------------------------------------}

type DemoQ = OutboundQ Msg_ Node

-- | Message
--
-- The @a@ parameter is the result of sending a message; we don't really use it
data Msg_ a = Msg {
      msgSender :: Node
    , msgType   :: MsgType
    , msgId     :: Int                -- ^ Unique identifier for this message
    , msgSend   :: Node -> Dequeue a  -- ^ Send the message
    }

type Msg = Msg_ ()

mkMsg :: Node -> MsgType -> Int -> Msg
mkMsg msgSender msgType msgId = msg
  where
    msg :: Msg
    msg = Msg{..}

    msgSend :: Node -> Dequeue ()
    msgSend them = liftIO $ sendSyncVar (nodeChan them) msg

instance Eq  Msg where (==) = (==) `on` msgId
instance Ord Msg where (<=) = (<=) `on` msgId

instance ClassifyMsg Msg_ where
  classifyMsg = msgType
  formatMsg = later $ \Msg{..} -> T.fromText . T.pack $ show (msgType, msgId)

instance ClassifyNode Node where
  classifyNode = nodeType . nodeCfg

{-------------------------------------------------------------------------------
  Model of a node

  We model a node as a thread that relays any message it had not previously
  received.
-------------------------------------------------------------------------------}

data NodeId = C Int | E Int | R Int
  deriving (Show, Eq, Ord)

data NodeCfg = NodeCfg {
      -- | Node ID (needed because the relayer wants an Ord instance)
      nodeId :: NodeId

      -- | Node type
    , nodeType :: NodeType

      -- | Delay on synchronous communication
      --
      -- Used to model slow nodes
    , nodeCommsDelay :: CommsDelay
    }

data Node = Node {
      nodeCfg     :: NodeCfg
    , nodeChan    :: SyncVar Msg
    , nodeMsgPool :: MVar (Set Msg)
    , nodeOutQ    :: DemoQ
    }

instance Eq   Node where (==) = (==) `on` (nodeId . nodeCfg)
instance Ord  Node where (<=) = (<=) `on` (nodeId . nodeCfg)
instance Show Node where show = show .    (nodeId . nodeCfg)

newNode :: NodeCfg -> IO Node
newNode nodeCfg@NodeCfg{..} = mdo
    nodeOutQ     <- OutQ.new node
                             (OutQ.defaultEnqueuePolicy nodeType)
                             (OutQ.defaultDequeuePolicy nodeType)
                             (OutQ.defaultFailurePolicy nodeType)
    _deqWorker   <- forkIO $ runDequeue $ OutQ.dequeueThread nodeOutQ msgSend
    nodeChan     <- newSyncVar
    nodeMsgPool  <- newMsgPool
    _deqListener <- forkIO $ runEnqueue $ forever $ do
      msg   <- recvSyncVar nodeChan nodeCommsDelay
      added <- addToMsgPool nodeMsgPool msg
      if added then do
        logNotice $ sformat (shown % ": received " % formatMsg) nodeId msg
        OutQ.enqueue nodeOutQ msg (OutQ.OriginForward (msgSender msg)) mempty
      else do
        logDebug $ sformat (shown % ": discarded " % formatMsg) nodeId msg
    let node = Node{..}
    return node

setPeers :: Node -> [Node] -> IO ()
setPeers Node{..} = OutQ.subscribe nodeOutQ . OutQ.simplePeers

send :: Msg -> Enqueue ()
send msg@Msg{msgSender = Node{nodeCfg = NodeCfg{..}, ..}} = do
    logNotice $ sformat (shown % ": enqueue " % formatMsg) nodeId msg
    liftIO $ modifyMVar_ nodeMsgPool $ \msgPool ->
               return $! Set.insert msg msgPool
    OutQ.enqueue nodeOutQ msg OutQ.OriginSender mempty

sendSync :: Msg -> Enqueue ()
sendSync msg@Msg{msgSender = Node{nodeCfg = NodeCfg{..}, ..}} = do
    logNotice $ sformat (shown % ": enqueueSync " % formatMsg) nodeId msg
    liftIO $ modifyMVar_ nodeMsgPool $ \msgPool ->
               return $! Set.insert msg msgPool
    OutQ.enqueueSync nodeOutQ msg OutQ.OriginSender mempty


{-------------------------------------------------------------------------------
  Message pool
-------------------------------------------------------------------------------}

type MsgPool = MVar (Set Msg)

newMsgPool :: MonadIO m => m MsgPool
newMsgPool = liftIO $ newMVar Set.empty

-- | Add a message to the pool
--
-- Returns whether the message was new.
addToMsgPool :: MonadIO m => MsgPool -> Msg -> m Bool
addToMsgPool pool msg = liftIO $ modifyMVar pool $ \msgs -> return $!
    if Set.member msg msgs
      then (msgs, False)
      else (Set.insert msg msgs, True)

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
