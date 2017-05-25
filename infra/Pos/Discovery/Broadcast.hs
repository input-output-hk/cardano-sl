-- | This module implements the capabilities of broadcasting info to
-- neighbors.
module Pos.Discovery.Broadcast
       ( converseToNeighbors
       ) where


import           Formatting                 (int, sformat, shown, (%))
import           Mockable                   (MonadMockable, forConcurrently, handleAll)
import           System.Wlog                (WithLogger, logDebug, logWarning)
import           Universum                  hiding (catchAll)

import           Pos.Communication.Protocol (Conversation, NodeId, SendActions (..))
import           Pos.Discovery.Class        (MonadDiscovery, getPeers)
import           Pos.Infra.Constants        (neighborsSendThreshold)

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
       )
    => SendActions m
    -> (NodeId -> NonEmpty (Conversation m ()))
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
