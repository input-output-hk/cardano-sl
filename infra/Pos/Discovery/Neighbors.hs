-- FIXME rename this. It's *not* about discovery, it's about broadcast.

module Pos.Discovery.Neighbors
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
import           Pos.Discovery.Constants    (neighborsSendThreshold)

-- | Send default message to neighbours in parallel.
-- It's a broadcasting to the neighbours without sessions
-- (i.e. we don't have to wait for reply from the listeners).
sendToNeighbors
    :: (MonadMockable m, Bi body, WithLogger m, Message body)
    => Set NodeId -> SendActions m -> body -> m ()
sendToNeighbors nodes_ sendActions msg = do
    nodes <- check nodes_
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
       , WithLogger m
       , Bi rcv
       , Bi snd
       , Message snd
       , Message rcv
       )
    => Set NodeId
    -> SendActions m
    -> (NodeId -> ConversationActions snd rcv m -> m ())
    -> m ()
converseToNeighbors nodes_ sendActions convHandler = do
    nodes <- check nodes_
    logDebug $ "converseToNeighbors: sending to nodes: " <> show nodes
    void $ forConcurrently nodes $ \node -> do
        handleAll (logErr node) $ withConnectionTo sendActions node (\_ -> convHandler node)
        logDebug $ "converseToNeighbors: DONE conversing to node " <> show node
    logDebug "converseToNeighbors: sending to nodes done"
  where
    logErr node e =
        logWarning $
        sformat ("Error in conversation to "%shown%": "%shown) node e
