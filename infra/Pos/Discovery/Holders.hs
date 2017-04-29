{-# LANGUAGE DataKinds #-}
-- | Transformer that carries peer discovery capabilities.

module Pos.Discovery.Holders
       ( DiscoveryConstT
       , runDiscoveryConstT
       , DiscoveryKademliaT
       , runDiscoveryKademliaT
       ) where

import qualified Control.Monad.Ether              as Ether
import qualified Control.Monad.Ether              as Ether.E
import           Data.Coerce                      (coerce)
import qualified Data.Set                         as S (fromList)
import           Mockable                         (Async, Catch, Mockables, Promise,
                                                   Throw)
import           System.Wlog                      (WithLogger)
import           Universum

import           Pos.Communication.Types.Protocol (NodeId)
import           Pos.DHT.Model                    (dhtNodeToNodeId, randomDHTKey)
import           Pos.DHT.Real                     (KademliaDHTInstance,
                                                   kademliaGetKnownPeers, kdiHandle,
                                                   lookupNode)
import           Pos.Discovery.Class              (MonadDiscovery (..))

----------------------------------------------------------------------------
-- Common
----------------------------------------------------------------------------

data DiscoveryTag -- loneliness is something we all know

----------------------------------------------------------------------------
-- Constant peers
----------------------------------------------------------------------------

type DiscoveryConstT m = Ether.E.ReaderT DiscoveryTag (Set NodeId) m

instance (Monad m) => MonadDiscovery (DiscoveryConstT m) where
    getPeers = Ether.E.ask (Proxy @DiscoveryTag)
    findPeers = pure mempty

runDiscoveryConstT :: (Set NodeId) -> DiscoveryConstT m a -> m a
runDiscoveryConstT = flip (Ether.runReaderT (Proxy @DiscoveryTag))

----------------------------------------------------------------------------
-- Kademlia DHT
----------------------------------------------------------------------------

type DiscoveryKademliaT m = Ether.E.ReaderT DiscoveryTag KademliaDHTInstance m

askDHTInstance
    :: (Ether.E.MonadReader DiscoveryTag KademliaDHTInstance m)
    => m KademliaDHTInstance
askDHTInstance = Ether.E.ask (Proxy @DiscoveryTag)

type DiscoveryKademliaEnv m =
    ( MonadIO m
    , Mockables m [ Async, Catch, Throw ]
    , Eq (Promise m (Maybe ()))
    , WithLogger m
    )

instance (DiscoveryKademliaEnv m) => MonadDiscovery (DiscoveryKademliaT m) where
    getPeers = do
        kademliaInstance <- askDHTInstance
        S.fromList . fmap dhtNodeToNodeId <$>
            kademliaGetKnownPeers kademliaInstance
    findPeers = do
        kademliaInstance <- askDHTInstance
        key <- randomDHTKey
        void $ liftIO $ lookupNode (kdiHandle kademliaInstance) key
        getPeers

runDiscoveryKademliaT
    :: (DiscoveryKademliaEnv m)
    => KademliaDHTInstance -> DiscoveryKademliaT m a -> m a
runDiscoveryKademliaT = flip (Ether.runReaderT (Proxy @DiscoveryTag))
