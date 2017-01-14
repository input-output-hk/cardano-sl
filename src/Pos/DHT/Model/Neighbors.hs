{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module Pos.DHT.Model.Neighbors
  ( sendToNeighbors
  , sendToNode
  ) where


import           Formatting          (int, sformat, shown, (%))
import           Mockable            (MonadMockable, forConcurrently, handleAll, throw)
import           Node                (SendActions (..))
import           Node.Message        (Message, Serializable)
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
sendToNeighbors sender msg = do
    nodes <- do
        nodes_ <- filterByNodeType DHTFull <$> getKnownPeers
        if length nodes_ < neighborsSendThreshold
           then discoverPeers DHTFull
           else return nodes_
    when (length nodes < neighborsSendThreshold) $
        logWarning $ sformat ("Send to only " % int % " nodes, threshold is " % int) (length nodes) (neighborsSendThreshold :: Int)
    void $ forConcurrently nodes $ \node -> handleAll (logSendErr node) $ sendToNode sender (dhtAddr node) msg
  where
    logSendErr node e = logWarning $ sformat ("Error sending to "%shown%": "%shown) node e

sendToNode
    :: ( MonadMockable m, Serializable packing body, Message body )
    => SendActions packing m
    -> NetworkAddress
    -> body
    -> m ()
sendToNode sender addr msg =
    handleAll handleE $ sendTo sender (addressToNodeId' 0 addr) msg
      where
        handleE e | isDevelopment = sendTo sender (addressToNodeId' 1 addr) msg
                  | otherwise     = throw e
