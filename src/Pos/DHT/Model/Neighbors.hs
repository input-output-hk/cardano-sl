{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module Pos.DHT.Model.Neighbors
  ( sendToNeighbors
  , sendToNode
  , converseToNeighbors
  , converseToNode
  ) where


import           Formatting          (int, sformat, shown, (%))
import           Mockable            (MonadMockable, forConcurrently, handleAll, throw)
import           Node                (ConversationActions, NodeId, SendActions (..))
import           Node.Message        (Message, Packable, Serializable, Unpackable)
import           System.Wlog         (WithLogger, logWarning)
import           Universum           hiding (catchAll, forConcurrently)

import           Pos.Constants       (neighborsSendThreshold)
import           Pos.Constants       (isDevelopment)
import           Pos.DHT.Model.Class (MonadDHT (..))
import           Pos.DHT.Model.Types (DHTNode (..), DHTNodeType (..), addressToNodeId',
                                      filterByNodeType)
import           Pos.Util.TimeWarp   (NetworkAddress)

-- | Send default message to neighbours in parallel.
-- It's a broadcasting to the neighbours without sessions
-- (i.e. we don't have to wait for reply from the listeners).
sendToNeighbors
    :: ( MonadDHT m, MonadMockable m, Serializable packing body, WithLogger m, Message body )
    => SendActions packing m
    -> body
    -> m ()
sendToNeighbors sendActions msg = do
    nodes <- getNodesWithCheck
    void $ forConcurrently nodes $ \node -> handleAll (logSendErr node) $ sendToNode sendActions (dhtAddr node) msg
  where
    logSendErr node e = logWarning $ sformat ("Error sending to "%shown%": "%shown) node e

getNodesWithCheck :: (MonadDHT m, WithLogger m) => m [DHTNode]
getNodesWithCheck = do
    nodes <- do
        nodes_ <- filterByNodeType DHTFull <$> getKnownPeers
        if length nodes_ < neighborsSendThreshold
           then discoverPeers DHTFull
           else return nodes_
    when (length nodes < neighborsSendThreshold) $
        logWarning $ sformat
            ("Send to only " % int % " nodes, threshold is " % int)
            (length nodes) (neighborsSendThreshold :: Int)
    return nodes

sendToNode
    :: ( MonadMockable m, Serializable packing body, Message body )
    => SendActions packing m
    -> NetworkAddress
    -> body
    -> m ()
sendToNode sendActions addr msg =
    handleAll handleE $ sendTo sendActions (addressToNodeId' 0 addr) msg
      where
        handleE e | isDevelopment = sendTo sendActions (addressToNodeId' 1 addr) msg
                  | otherwise     = throw e

converseToNode
    :: ( MonadMockable m, Unpackable packing rcv, Packable packing snd, Message snd )
    => SendActions packing m
    -> NetworkAddress
    -> (NodeId -> ConversationActions snd rcv m -> m t)
    -> m t
converseToNode sendActions addr handler =
    handleAll handleE $ tryConnect 0
      where
        handleE e | isDevelopment = tryConnect 1
                  | otherwise     = throw e
        tryConnect i = withConnectionTo sendActions peerId $ handler peerId
            where
              peerId = addressToNodeId' i addr

converseToNeighbors
    :: ( MonadDHT m, MonadMockable m, WithLogger m, Unpackable packing rcv, Packable packing snd, Message snd )
    => SendActions packing m
    -> (NodeId -> ConversationActions snd rcv m -> m ())
    -> m ()
converseToNeighbors sendActions convHandler = do
    nodes <- getNodesWithCheck
    void $ forConcurrently nodes $ \node -> handleAll (logErr node) $ converseToNode sendActions (dhtAddr node) convHandler
  where
    logErr node e = logWarning $ sformat ("Error in conversation to "%shown%": "%shown) node e
