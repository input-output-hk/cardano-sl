
module Pos.DHT.Model.Neighbors
       ( sendToNeighbors
       , sendToNode
       , converseToNeighbors
       , converseToNode
       ) where


import           Formatting                 (int, sformat, shown, (%))
import           Mockable                   (MonadMockable, forConcurrently, handleAll,
                                             throw)
import           System.Wlog                (WithLogger, logDebug, logWarning)
import           Universum                  hiding (catchAll)

import           Pos.Binary.Class           (Bi)
import           Pos.Communication.Protocol (ConversationActions, Message, NodeId (..),
                                             PeerId (..), SendActions (..))
import           Pos.Constants              (neighborsSendThreshold)
import           Pos.Constants              (isDevelopment)
import           Pos.DHT.Model.Class        (MonadDHT (..))
import           Pos.DHT.Model.Types        (DHTNode (..), getMeaningPart)
import           Pos.Util.TimeWarp          (addressToNodeId')

-- | Send default message to neighbours in parallel.
-- It's a broadcasting to the neighbours without sessions
-- (i.e. we don't have to wait for reply from the listeners).
sendToNeighbors
    :: (MonadDHT m, MonadMockable m, Bi body, WithLogger m, Message body)
    => SendActions m -> body -> m ()
sendToNeighbors sendActions msg = do
    nodes <- getNodesWithCheck
    void $
        forConcurrently nodes $ \node ->
            handleAll (logSendErr node) $ sendToNode sendActions node msg
  where
    logSendErr node e =
        logWarning $ sformat ("Error sending to " %shown % ": " %shown) node e

getNodesWithCheck :: (MonadDHT m, WithLogger m) => m [DHTNode]
getNodesWithCheck = do
    nodes <- do
        nodes_ <- getKnownPeers
        if length nodes_ < neighborsSendThreshold
           then discoverPeers
           else return nodes_
    when (length nodes < neighborsSendThreshold) $
        logWarning $ sformat
            ("Send to only " % int % " nodes, threshold is " % int)
            (length nodes) (neighborsSendThreshold :: Int)
    return nodes

sendToNode
    :: (MonadMockable m, Bi body, Message body)
    => SendActions m -> DHTNode -> body -> m ()
sendToNode sendActions node msg =
    handleAll handleE $ trySend 0
      where
        handleE e | isDevelopment = trySend 1
                  | otherwise     = throw e
        trySend i = sendTo sendActions (toNodeId i node) msg

converseToNode
    :: (MonadMockable m, Bi rcv, Bi snd, Message snd, Message rcv)
    => SendActions m
    -> DHTNode
    -> (NodeId -> ConversationActions snd rcv m -> m t)
    -> m t
converseToNode sendActions node handler =
    handleAll handleE $ tryConnect 0
  where
    handleE e | isDevelopment = tryConnect 1
              | otherwise     = throw e
    tryConnect i =
        let nodeId = toNodeId i node
        in withConnectionTo sendActions nodeId $ \_peerData -> handler nodeId


toNodeId :: Word32 -> DHTNode -> NodeId
toNodeId i DHTNode {..} = NodeId $ (peerId, addressToNodeId' i dhtAddr)
  where
    peerId = PeerId $ getMeaningPart dhtNodeId


converseToNeighbors
    :: ( MonadDHT m
       , MonadMockable m
       , WithLogger m
       , Bi rcv
       , Bi snd
       , Message snd
       , Message rcv
       )
    => SendActions m
    -> (NodeId -> ConversationActions snd rcv m -> m ())
    -> m ()
converseToNeighbors sendActions convHandler = do
    nodes <- getNodesWithCheck
    logDebug $ "converseToNeighbors: sending to nodes: " <> show nodes
    void $ forConcurrently nodes $ \node -> do
        handleAll (logErr node) $ converseToNode sendActions node convHandler
        logDebug $ "converseToNeighbors: DONE conversing to node " <> show node
    logDebug "converseToNeighbors: sending to nodes done"
  where
    logErr node e =
        logWarning $
        sformat ("Error in conversation to "%shown%": "%shown) node e
