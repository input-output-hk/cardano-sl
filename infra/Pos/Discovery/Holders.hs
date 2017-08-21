{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

-- | Transformer that carries peer discovery capabilities.

module Pos.Discovery.Holders
       ( DiscoveryContextSum (..)
       , HasDiscoveryContextSum (..)
       , discoveryWorkers
       , getPeersSum
       , findPeersSum
       ) where

import           Universum

import qualified Data.Set                         as S (fromList)
import           Mockable                         (Async, Catch, Mockables, Promise,
                                                   Throw)
import           System.Wlog                      (WithLogger)

import           Pos.Communication.Types.Protocol (ActionSpec (..), NodeId, OutSpecs,
                                                   WorkerSpec)
import           Pos.DHT.Model                    (randomDHTKey)
import           Pos.DHT.Real                     (KademliaDHTInstance,
                                                   kademliaGetKnownPeers, kdiHandle,
                                                   lookupNode)
import           Pos.DHT.Workers                  (DhtWorkMode, dhtWorkers)
import           Pos.Recovery.Info                (MonadRecoveryInfo, recoveryCommGuard)
import           Pos.Util.TimeWarp                (addressToNodeId)

----------------------------------------------------------------------------
-- Kademlia DHT
----------------------------------------------------------------------------

type DiscoveryKademliaEnv m =
    ( MonadIO m
    , Mockables m [ Async, Catch, Throw ]
    , Eq (Promise m (Maybe ()))
    , WithLogger m
    )

getPeersKademlia :: DiscoveryKademliaEnv m => KademliaDHTInstance -> m (Set NodeId)
getPeersKademlia kademliaInstance = do
    S.fromList . fmap addressToNodeId <$>
        kademliaGetKnownPeers kademliaInstance

findPeersKademlia :: DiscoveryKademliaEnv m => KademliaDHTInstance -> m (Set NodeId)
findPeersKademlia kademliaInstance = do
    key <- randomDHTKey
    void $ liftIO $ lookupNode (kdiHandle kademliaInstance) key
    getPeersKademlia kademliaInstance

----------------------------------------------------------------------------
-- Sum
----------------------------------------------------------------------------

-- | Context of Discovery implementation. It's basically a sum of all
-- possible contexts.
data DiscoveryContextSum
    = DCStatic !(Set NodeId)
    | DCKademlia !KademliaDHTInstance

class HasDiscoveryContextSum ctx where
    discoveryContextSum :: Lens' ctx DiscoveryContextSum

type DiscoverySumEnv ctx m =
    (MonadReader ctx m, HasDiscoveryContextSum ctx, DiscoveryKademliaEnv m)

getPeersSum :: DiscoverySumEnv ctx m => m (Set NodeId)
getPeersSum =
    view discoveryContextSum >>= \case
        DCStatic nodes -> return nodes
        DCKademlia inst -> getPeersKademlia inst

findPeersSum :: DiscoverySumEnv ctx m => m (Set NodeId)
findPeersSum =
    view discoveryContextSum >>= \case
        DCStatic nodes -> return nodes
        DCKademlia inst -> findPeersKademlia inst

-- | Get all discovery workers using 'DiscoveryContextSum'.
discoveryWorkers ::
       forall m ctx. (MonadRecoveryInfo m, DhtWorkMode ctx m)
    => DiscoveryContextSum
    -> ([WorkerSpec m], OutSpecs)
discoveryWorkers ctx =
    first (map convertWorker) $
    case ctx of
        DCStatic _     -> mempty
        DCKademlia var -> dhtWorkers var
  where
    convertWorker :: WorkerSpec m -> WorkerSpec m
    convertWorker (ActionSpec worker) =
        ActionSpec $ \verInfo sendActions ->
            recoveryCommGuard (worker verInfo sendActions)
