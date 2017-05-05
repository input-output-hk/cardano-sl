{-# LANGUAGE RankNTypes #-}

-- | Abstraction over possibility to retrieve neighbors.

module Pos.Discovery.Class
       ( MonadDiscovery (..)
       ) where

import           Control.Monad.Trans              (MonadTrans)
import           Universum

import           Pos.Communication.Types.Protocol (NodeId)

-- | Monads that can hold information about neighbors and allow
-- searching for other nodes.
class (Monad m) => MonadDiscovery m where
    -- | Retrieve current peers set. Should be O(1) operation without
    -- any computations.
    getPeers  :: m (Set NodeId)
    -- | Perform a search for new peers (blocking) and return a new
    -- set.
    findPeers :: m (Set NodeId)

instance {-# OVERLAPPABLE #-}
    (MonadDiscovery m, MonadTrans t, Monad (t m)) => MonadDiscovery (t m) where
    getPeers = lift $ getPeers
    findPeers = lift $ findPeers
