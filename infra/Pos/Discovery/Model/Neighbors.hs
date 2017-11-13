module Pos.Discovery.Model.Neighbors
       ( sendToNeighbors
       , converseToNeighbors
       ) where

import           Formatting (sformat, shown, (%))
import           Mockable (MonadMockable, handleAll)
import           Pos.Binary.Class (Bi)
import           Pos.Communication.Protocol (ConversationActions, Message, NodeId (..),
                                             SendActions (..))
import           Pos.Discovery.Model.Class (Discovery (..), withPeersConcurrently)
import           System.Wlog (WithLogger, logDebug, logWarning)
import           Universum

sendToNeighbors
    :: ( Discovery which m
       , MonadMockable m
       , Bi body
       , WithLogger m
       , Message body
       )
    => Proxy which
    -> SendActions m
    -> body
    -> m ()
sendToNeighbors which sendActions msg = do
    void $ withPeersConcurrently which $ \nodes -> do
        logDebug $ "sendToNeighbors: sending to nodes: " <> show nodes
        return (\node -> handleAll (logSendErr node) $ sendTo sendActions node msg)
    logDebug "sendToNeighbors: sending to nodes done"
    where
    logSendErr node e =
        logWarning $ sformat ("Error sending to " % shown % ": " % shown) node e

converseToNeighbors
    :: ( Discovery which m
       , MonadMockable m
       , WithLogger m
       , Bi rcv
       , Bi snd
       , Message snd
       , Message rcv
       )
    => Proxy which
    -> SendActions m
    -> (NodeId -> ConversationActions snd rcv m -> m ())
    -> m ()
converseToNeighbors which  sendActions handler = do
    void $ withPeersConcurrently which $ \nodes -> do
        logDebug $ "converseToNeighbors: sending to nodes: " <> show nodes
        return $ \node -> do
            handleAll (logErr node) $ withConnectionTo sendActions node $ \_ -> handler node
            logDebug $ "converseToNeighbors: DONE conversing to node " <> show node
    logDebug "converseToNeighbors: sending to nodes done"
    where
    logErr node e = logWarning $ sformat ("Error in conversation to " % shown % ": " % shown) node e
