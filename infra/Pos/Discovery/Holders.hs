{-# LANGUAGE DataKinds #-}

-- | Transformer that carries peer discovery capabilities.

module Pos.Discovery.Holders
       ( DiscoveryTag
       , DiscoveryConstT
       , runDiscoveryConstT
       , DiscoveryKademliaT
       , askDHTInstance
       , runDiscoveryKademliaT
       ) where

import           Universum

import qualified Data.Set                         as S (fromList)
import qualified Ether
import           Mockable                         (Async, Catch, Mockables, Promise,
                                                   Throw)
import           System.Wlog                      (WithLogger)

import           Pos.Communication.Types.Protocol (NodeId)
import           Pos.DHT.Model                    (randomDHTKey)
import           Pos.DHT.Real                     (KademliaDHTInstance,
                                                   kademliaGetKnownPeers, kdiHandle,
                                                   lookupNode)
import           Pos.Discovery.Class              (MonadDiscovery (..))
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
