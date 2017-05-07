-- | This module implements the capabilities of broadcasting info to
-- neighbors.
module Pos.Discovery.Broadcast
       ( sendToNeighbors
       , converseToNeighbors
       ) where


import           Formatting                 (int, sformat, shown, (%))
import           Mockable                   (MonadMockable, forConcurrently, handleAll)
import           System.Wlog                (WithLogger, logDebug, logWarning)
import           Universum                  hiding (catchAll)

import           Pos.Binary.Class           (Bi)
import           Pos.Communication.Protocol (ConversationActions, Message, NodeId (..),
                                             SendActions (..))
import           Pos.Discovery.Class        (MonadDiscovery, getPeers)
import           Pos.Infra.Constants        (neighborsSendThreshold)

-- | Send default message to neighbours in parallel.
-- It's a broadcasting to the neighbours without sessions
-- (i.e. we don't have to wait for reply from the listeners).
sendToNeighbors
    :: ( MonadMockable m
       , WithLogger m
       , MonadDiscovery m
       , Message body
       , Bi body
       )
    => SendActions m -> body -> m ()
sendToNeighbors sendActions msg = do
    nodes <- check =<< getPeers
    void $
        forConcurrently nodes $ \node ->
            handleAll (logSendErr node) $ sendTo sendActions node msg
  where
    logSendErr node e =
        logWarning $ sformat ("Error sending to " %shown % ": " %shown) node e

check :: (WithLogger m) => Set NodeId -> m [NodeId]
check nodes = do
    when (length nodes < neighborsSendThreshold) $
        logWarning $ sformat
            ("Send to only " % int % " nodes, threshold is " % int)
            (length nodes) (neighborsSendThreshold :: Int)
    return (toList nodes)

converseToNeighbors
    :: ( MonadMockable m
       , MonadDiscovery m
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
    nodes <- check =<< getPeers
    logDebug $ "converseToNeighbors: sending to nodes: " <> show nodes
    void $ forConcurrently nodes $ \node -> do
        handleAll (logErr node) $ withConnectionTo sendActions node (\_ -> convHandler node)
        logDebug $ "converseToNeighbors: DONE conversing to node " <> show node
    logDebug "converseToNeighbors: sending to nodes done"
  where
    logErr node e =
        logWarning $
        sformat ("Error in conversation to "%shown%": "%shown) node e
