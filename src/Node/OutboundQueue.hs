{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Node.OutboundQueue
    ( OutboundQueue (..)
    , freeForAll
    ) where

import           Control.Monad (forM)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Map (Map)
import qualified Data.Map as M
import           Mockable.Class
import           Mockable.Concurrent
import qualified Node.Internal as LL
import           Node.Conversation

data OutboundQueue packingType peerData peer msgClass m = OutboundQueue
    { oqEnqueue
          :: forall t .
             Set peer
          -> msgClass
          -> (peer -> peerData -> Conversation packingType m t)
          -> m (Map peer (m t))
    , oqClose :: m ()
    }

-- | An outbound queue which doesn't do any queueing. Calling oqEnqueue will
--   immediately do a conversation to each of the peers, concurrently.
freeForAll
    :: forall packingType peerData peer msgClass m .
       ( Monad m, Mockable Async m, Ord peer )
    => (peer -> LL.NodeId)
    -> Converse packingType peerData m
    -> OutboundQueue packingType peerData peer msgClass m
freeForAll nodeId converse = OutboundQueue
    { oqEnqueue = enqueue
    , oqClose = pure ()
    }
  where
    enqueue
        :: forall t .
           Set peer
        -> msgClass
        -> (peer -> peerData -> Conversation packingType m t)
        -> m (Map peer (m t))
    enqueue peers _ k = do
        -- TODO not quite right. We shouldn't wait until they're all done to
        -- return the map, as this makes asynchronous enqueueing impossible.
        lst <- forM (S.toList peers) $ \peer -> do
            it <- async $ converse (nodeId peer) (k peer)
            return (peer, wait it)
        return $ M.fromList lst
