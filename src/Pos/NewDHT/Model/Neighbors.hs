{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module Pos.NewDHT.Model.Neighbors where

import           Control.Monad                        (sequence)
import           Control.Monad.Catch                  (SomeException, catch)
import           Data.ByteString.Char8                (pack)

import           Data.Foldable                        (notElem)
import           Formatting                           (int, sformat, shown, (%))
import qualified Formatting                           as F
import           Message.Message                      (Message, Serializable)
import           Mockable.Monad                       (MonadMockable)
import           Network.Discovery.Transport.Kademlia
import qualified Network.Transport.Abstract           as NT
import           Node                                 (NodeId (..), SendActions (..))
import           System.Wlog                          (WithLogger, logInfo, logWarning)
import           Universum

import           Pos.Constants                        (neighborsSendThreshold)
import           Pos.NewDHT.Model.Class.BiP
import           Pos.NewDHT.Model.Class.MonadDHT      (MonadDHT (..))
import           Pos.NewDHT.Model.Types               (DHTNode (..), DHTNodeType (..),
                                                       filterByNodeType)

-- | Send default message to neighbours in parallel.
-- It's a broadcasting to the neighbours without sessions
-- (i.e. we don't have to wait for reply from the listeners).
sendToNeighbors
    :: ( MonadDHT m, MonadMockable m, Serializable packing body, WithLogger m, Message body, MonadCatch m )
    => SendActions packing state m
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
    send' node = sendTo sender anId msg `catch` handleE
      where
        (host, port) = dhtAddr node
        -- TODO: What about node index, i.e. last number in '127.0.0.1:3000:0' ?
        anId = NodeId . NT.EndPointAddress $ host <> ":" <> (pack . show $ port)
        handleE (e :: SomeException) = do
            logInfo $ sformat ("Error sending message to " % F.build % ": " % shown) node e
