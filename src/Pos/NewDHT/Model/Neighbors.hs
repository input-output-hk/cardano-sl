{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module Pos.NewDHT.Model.Neighbors
  ( sendToNeighbors
  ) where

import           Control.Monad          (sequence)
import           Data.ByteString.Char8  (pack)

import           Data.Foldable          (notElem)
import           Formatting             (int, sformat, shown, (%))
import qualified Formatting             as F
import           Message.Message        (Message, Serializable)
import           Mockable               (MonadMockable, catchAll)
import           Node                   (NodeId (..), SendActions (..))
import           System.Wlog            (WithLogger, logInfo, logWarning)
import           Universum              hiding (catchAll)

import           Pos.Constants          (neighborsSendThreshold)
import           Pos.NewDHT.Model.Class (MonadDHT (..))
import           Pos.NewDHT.Model.Types (DHTNode (..), DHTNodeType (..), addressToNodeId,
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
        logWarning $ sformat ("Send to only " % int % " nodes, threshold is " % int) (length nodes) neighborsSendThreshold
    -- We don't need to parallelize sends here, because they are asynchronous by design
    mapM_ send' nodes
  where
    send' node = sendTo sender (addressToNodeId . dhtAddr $ node) msg `catchAll` handleE
      where
        handleE e = do
            logInfo $ sformat ("Error sending message to " % F.build % ": " % shown) node e
