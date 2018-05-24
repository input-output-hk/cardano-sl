module Pos.Discovery.Model.Class
       ( Discovery(..)
       , withPeers
       , withPeersConcurrently
       ) where

import           Data.Proxy (Proxy)
import           Data.Set (Set)
import qualified Data.Set as Set (toList)
import           Mockable (Mockable)
import           Mockable.Concurrent (Concurrently, forConcurrently)
import           Pos.Communication.Protocol (NodeId)
import           Universum

-- | Provides a set of known peers.
class Discovery which m where
    peers :: Proxy which -> m (Set NodeId)

withPeers
    :: ( Discovery which m
       , Functor m
       )
    => Proxy which
    -> (Set NodeId -> r)
    -> m r
withPeers which k = fmap k (peers which)

-- | Run an m-term against each peer, concurrently.
--   The target monad must be capable of concurrency.
withPeersConcurrently
    :: ( Discovery which m
       , Mockable Concurrently m
       )
    => Proxy which
    -> (Set NodeId -> m (NodeId -> m t))
    -> m [t]
withPeersConcurrently which k = join . withPeers which $ \nodes -> do
    f <- k nodes
    forConcurrently (Set.toList nodes) f

{-
-- A type and instance for a discovery backed by a static set of NodeIds.
-- Abstract discovery types can make use of this by making this instance
--
--
--   instance
--       ( Discovery (StaticDiscovery MyAbstractDiscovery) m
--       ) => Discovery MyAbstractDiscocvery m
--       where
--       peers _ = peers (Proxy :: Proxy (StaticDiscovery MyAbstractDiscovery))
--
-- which will of course induce the constraints
--
--   ( Monad m, Configures config (StaticDiscovery MyAbstractDiscovery) (Set NodeId) )
--
-- So the programmer will also have to choose an appropriate config and give
-- the configures instance for it.
--
data StaticDiscovery t

instance
    ( Monad m
    , Configures config (StaticDiscovery t) (Set NodeId)
    ) => Discovery (StaticDiscovery t) (ReaderT config m)
    where
    peers _ = asks (getConfig (Proxy :: Proxy (StaticDiscovery t)))


class Concurrency (m :: * -> *) where
    data Async m :: * -> *
    data ThreadId m :: *
    async :: m t -> m (Async m t)
    withAsync :: m x -> (Async m x -> m t) -> m t
    wait :: Promise m t -> m t
    forConcurrently :: [x] -> (x -> m t) -> m [t]
    asyncThreadId :: Async m -> ThreadId m
-}
