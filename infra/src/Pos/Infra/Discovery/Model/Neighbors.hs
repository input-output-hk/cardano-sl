module Pos.Infra.Discovery.Model.Neighbors
       ( sendToNeighbors
       , converseToNeighbors
       ) where

import           Formatting (sformat, shown, (%))
import           Mockable (MonadMockable, handleAll)
import           Pos.Binary.Class (Bi)
import           Pos.Infra.Communication.Protocol (ConversationActions, Message,
                     NodeId (..), SendActions (..))
import           Pos.Infra.Discovery.Model.Class (Discovery (..),
                     withPeersConcurrently)
import           Pos.Util.Trace.Named (TraceNamed, logDebug, logWarning)
import           Universum

sendToNeighbors
    :: ( Discovery which m
       , MonadMockable m
       , Bi body
       , Message body
       )
    => TraceNamed m
    -> Proxy which
    -> SendActions m
    -> body
    -> m ()
sendToNeighbors logTrace which sendActions msg = do
    void $ withPeersConcurrently which $ \nodes -> do
        logDebug logTrace $ "sendToNeighbors: sending to nodes: " <> show nodes
        return (\node -> handleAll (logSendErr node) $ sendTo sendActions node msg)
    logDebug logTrace "sendToNeighbors: sending to nodes done"
    where
    logSendErr node e =
        logWarning  logTrace $ sformat ("Error sending to " % shown % ": " % shown) node e

converseToNeighbors
    :: ( Discovery which m
       , MonadMockable m
       , Bi rcv
       , Bi snd
       , Message snd
       , Message rcv
       )
    => TraceNamed m
    -> Proxy which
    -> SendActions m
    -> (NodeId -> ConversationActions snd rcv m -> m ())
    -> m ()
converseToNeighbors logTrace which sendActions handler = do
    void $ withPeersConcurrently which $ \nodes -> do
        logDebug logTrace $ "converseToNeighbors: sending to nodes: " <> show nodes
        return $ \node -> do
            handleAll (logErr node) $ withConnectionTo sendActions node $ \_ -> handler node
            logDebug logTrace $ "converseToNeighbors: DONE conversing to node " <> show node
    logDebug logTrace "converseToNeighbors: sending to nodes done"
    where
      logErr node e = logWarning logTrace $ sformat ("Error in conversation to " % shown % ": " % shown) node e
