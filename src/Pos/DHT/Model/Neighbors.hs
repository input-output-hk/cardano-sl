{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module Pos.DHT.Model.Neighbors
  ( sendToNeighbors
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
    mapM_ send' nodes
  where
    send' node = handleAll handleE $ do
                    sendTo sender (addressToNodeId' 0 . dhtAddr $ node) msg
      where
        handleE e = do
            logInfo $ sformat ("Error sending message to " % F.build % " (endpoint 0): " % shown) node e
            -- [CSL-447] temporary solution, need a proper fix probably
            -- Solution: maintain state per network-address, most-known endpoint?
            -- But I bet we can request available endpoints or whatever
            -- Though we don't need it for production, so maintaining shared variable in state works
            when isDevelopment . handleAll handleE1 $
                sendTo sender (addressToNodeId' 1 . dhtAddr $ node) msg
        handleE1 e = do
            logInfo $ sformat ("Error sending message to " % F.build % " (endpoint 1): " % shown) node e
