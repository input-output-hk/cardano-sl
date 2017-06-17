{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

-- | Transformer that carries peer discovery capabilities.

module Pos.Discovery.Holders
       ( DiscoveryTag
       , DiscoveryConstT
       , runDiscoveryConstT
       , DiscoveryKademliaT
       , askDHTInstance
       , runDiscoveryKademliaT

       , DiscoveryContextSum (..)
       , MonadDiscoverySum
       , DiscoveryRedirect
       , askDiscoveryContextSum
       , runDiscoveryRedirect
       , discoveryWorkers
       ) where

import           Universum

import           Control.Monad.Trans.Identity     (IdentityT (..))
import           Data.Coerce                      (coerce)
import qualified Data.Set                         as S (fromList)
import qualified Ether
import           Mockable                         (Async, Catch, Mockables, Promise,
                                                   Throw)
import           System.Wlog                      (WithLogger)

import           Pos.Communication.Types.Protocol (NodeId, OutSpecs, WorkerSpec)
import           Pos.DHT.Model                    (randomDHTKey)
import           Pos.DHT.Real                     (KademliaDHTInstance,
                                                   kademliaGetKnownPeers, kdiHandle,
                                                   lookupNode)
import           Pos.DHT.Workers                  (DhtWorkMode, dhtWorkers)
import           Pos.Discovery.Class              (MonadDiscovery (..))
import           Pos.Recovery.Info                (MonadRecoveryInfo,
                                                   recoveryCommGuardSimple)
import           Pos.Util.TimeWarp                (addressToNodeId)

----------------------------------------------------------------------------
-- Common
----------------------------------------------------------------------------

data DiscoveryTag -- loneliness is something we all know

----------------------------------------------------------------------------
-- Constant peers
----------------------------------------------------------------------------

-- | 'MonadDiscovery' capable transformer that uses a constant/static
-- set of peers and doesn't do anything on 'findPeers' call.
type DiscoveryConstT m = Ether.ReaderT DiscoveryTag (Set NodeId) m

instance (Monad m) => MonadDiscovery (DiscoveryConstT m) where
    getPeers = Ether.ask @DiscoveryTag
    findPeers = getPeers

runDiscoveryConstT :: (Set NodeId) -> DiscoveryConstT m a -> m a
runDiscoveryConstT = flip (Ether.runReaderT @DiscoveryTag)

----------------------------------------------------------------------------
-- Kademlia DHT
----------------------------------------------------------------------------

-- | Transformer that captures the Kademlia DHT functionality
-- inside. Its 'MonadDiscovery' instance performs a node lookup on
-- 'findPeers'.
type DiscoveryKademliaT m = Ether.ReaderT DiscoveryTag KademliaDHTInstance m

askDHTInstance
    :: (Ether.MonadReader DiscoveryTag KademliaDHTInstance m)
    => m KademliaDHTInstance
askDHTInstance = Ether.ask @DiscoveryTag

type DiscoveryKademliaEnv m =
    ( MonadIO m
    , Mockables m [ Async, Catch, Throw ]
    , Eq (Promise m (Maybe ()))
    , WithLogger m
    )

instance (DiscoveryKademliaEnv m) => MonadDiscovery (DiscoveryKademliaT m) where
    getPeers = do
        kademliaInstance <- askDHTInstance
        S.fromList . fmap addressToNodeId <$>
            kademliaGetKnownPeers kademliaInstance
    findPeers = do
        kademliaInstance <- askDHTInstance
        key <- randomDHTKey
        void $ liftIO $ lookupNode (kdiHandle kademliaInstance) key
        getPeers

runDiscoveryKademliaT
    :: (DiscoveryKademliaEnv m)
    => KademliaDHTInstance -> DiscoveryKademliaT m a -> m a
runDiscoveryKademliaT = flip (Ether.runReaderT @DiscoveryTag)

----------------------------------------------------------------------------
-- Sum
----------------------------------------------------------------------------

-- | Context of Discovery implementation. It's basically a sum of all
-- possible contexts.
data DiscoveryContextSum
    = DCStatic !(Set NodeId)
    | DCKademlia !KademliaDHTInstance

-- | Monad which combines all 'MonadDiscovery' implementations (and
-- uses only one of them).
type MonadDiscoverySum = Ether.MonadReader' DiscoveryContextSum

data DiscoveryRedirectTag

type DiscoveryRedirect =
    Ether.TaggedTrans DiscoveryRedirectTag IdentityT

runDiscoveryRedirect :: DiscoveryRedirect m a -> m a
runDiscoveryRedirect = coerce

askDiscoveryContextSum :: MonadDiscoverySum m => m DiscoveryContextSum
askDiscoveryContextSum = Ether.ask'

instance (MonadDiscoverySum m, DiscoveryKademliaEnv m, t ~ IdentityT) =>
         MonadDiscovery (Ether.TaggedTrans DiscoveryRedirectTag t m) where
    getPeers =
        Ether.ask' >>= \case
            DCStatic nodes -> runDiscoveryConstT nodes getPeers
            DCKademlia inst -> runDiscoveryKademliaT inst getPeers
    findPeers =
        Ether.ask' >>= \case
            DCStatic nodes -> runDiscoveryConstT nodes findPeers
            DCKademlia inst -> runDiscoveryKademliaT inst findPeers

-- | Get all discovery workers using 'DiscoveryContextSum'.
discoveryWorkers ::
       (MonadRecoveryInfo m, DhtWorkMode m)
    => DiscoveryContextSum
    -> ([WorkerSpec m], OutSpecs)
discoveryWorkers ctx =
    first (map recoveryCommGuardSimple) $
    case ctx of
        DCStatic _     -> mempty
        DCKademlia var -> dhtWorkers var
