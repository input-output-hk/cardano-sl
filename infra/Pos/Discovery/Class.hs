{-# LANGUAGE RankNTypes #-}

-- | Abstraction over possibility to retrieve neighbors.

module Pos.Discovery.Class
       ( MonadDiscovery (..)
       ) where

import           Control.Monad.Trans              (MonadTrans)
import           Pos.Communication.Types.Protocol (NodeId)
import           Universum

class (Monad m) => MonadDiscovery m where
    getPeers  :: m (Set NodeId)
    findPeers :: m (Set NodeId)

instance {-# OVERLAPPABLE #-}
    (MonadDiscovery m, MonadTrans t, Monad (t m)) => MonadDiscovery (t m) where
    getPeers = lift $ getPeers
    findPeers = lift $ findPeers
