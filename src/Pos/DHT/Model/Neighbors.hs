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
import qualified Formatting          as F
import           Mockable            (MonadMockable, handleAll)
import           Node                (SendActions (..))
import           Node.Message        (Message, Serializable)
import           System.Wlog         (WithLogger, logInfo, logWarning)
import           Universum           hiding (catchAll)

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
    -- We don't need to parallelize sends here, because they are asynchronous by design
    forM_ nodes $ \node -> sendToNode sender (dhtAddr node) msg

sendToNode
    :: ( MonadMockable m, Serializable packing body, WithLogger m, Message body )
    => SendActions packing m
    -> NetworkAddress
    -> body
    -> m ()
sendToNode sender addr msg =
    handleAll handleE $ sendTo sender (addressToNodeId' 0 addr) msg
      where
        handleE e = do
            logInfo $ sformat ("Error sending message to " % F.shown % " (endpoint 0): " % shown) addr e
            -- [CSL-447] temporary solution, need a proper fix probably
            -- Solution: maintain state per network-address, most-known endpoint?
            -- But I bet we can request available endpoints or whatever
            -- Though we don't need it for production, so maintaining shared variable in state works
            when isDevelopment . handleAll handleE1 $
                sendTo sender (addressToNodeId' 1 addr) msg
        handleE1 e = do
            logInfo $ sformat ("Error sending message to " % F.shown % " (endpoint 1): " % shown) addr e
