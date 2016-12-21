-- | This module contains in-memory implementation of Kademlia P2P-discovery.

module Network.Discovery.Transport.InMemory
       ( InMemoryDiscoveryErrorCode
       , inMemoryDiscovery
       ) where

import qualified Control.Concurrent.STM              as STM
import qualified Control.Concurrent.STM.TVar         as TVar
import           Control.Monad.IO.Class              (MonadIO, liftIO)
import qualified Data.Map                            as M
import           Data.Set                            (Set)
import qualified Data.Set                            as S
import           Data.Typeable                       (Typeable)
import           GHC.Generics                        (Generic)
import           Network.Transport.InMemory
import           Network.Transport.InMemory.Internal (TransportState (..),
                                                      ValidTransportState (..))

import           Network.Discovery.Abstract
import           Network.Transport

-- | An InMemory network transport can be used as a NetworkDiscovery. It takes
--   addresses from the transport's internal state.
inMemoryDiscovery
    :: ( MonadIO m )
    => TransportInternals
    -> m (NetworkDiscovery InMemoryDiscoveryErrorCode m)
inMemoryDiscovery internals = do
    peersTVar <- liftIO . TVar.newTVarIO $ S.empty
    let knownPeers = liftIO . TVar.readTVarIO $ peersTVar
    let discoverPeers = liftIO . STM.atomically $ do
            mAllPeers <- getPeersFromInternals internals
            case mAllPeers of
                Nothing -> pure (Left (DiscoveryError InMemoryDiscoveryTransportClosed "Transport is closed"))
                Just allPeers -> do
                    knownPeers <- TVar.readTVar peersTVar
                    let newPeers = allPeers `S.difference` knownPeers
                    TVar.writeTVar peersTVar allPeers
                    pure (Right newPeers)
    -- No need to do anything on close, since we don't exactly use any
    -- resources.
    let close = pure ()
    pure $ NetworkDiscovery knownPeers discoverPeers close

getPeersFromInternals :: TransportInternals -> STM.STM (Maybe (Set EndPointAddress))
getPeersFromInternals (TransportInternals tvar) = do
    state <- TVar.readTVar tvar
    case state of
        TransportValid (ValidTransportState map _) ->
            pure . Just $ S.fromList (M.keys map)
        TransportClosed -> pure Nothing

data InMemoryDiscoveryErrorCode = InMemoryDiscoveryTransportClosed
  deriving (Show, Typeable, Generic)
