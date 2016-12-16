{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Discovery.Transport.Kademlia (

      K.Node(..)
    , K.Peer(..)
    , KademliaConfiguration(..)
    , kademliaDiscovery
    , KademliaDiscoveryErrorCode(..)

    ) where

import GHC.Generics (Generic)
import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Typeable (Typeable)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word16)
import Data.Binary (encode, decodeOrFail, Binary)
import Network.Discovery.Abstract
import qualified Network.Kademlia as K
import Network.Transport
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar

newtype KSerialize i = KSerialize i
    deriving (Eq, Ord, Show)

instance Binary i => K.Serialize (KSerialize i) where
    fromBS bs = case decodeOrFail (BL.fromStrict bs) of
        Left (_, _, str) -> Left str
        Right (unconsumed, _, i) -> Right (KSerialize i, BL.toStrict unconsumed)
    toBS (KSerialize i) = BL.toStrict . encode $ i

data KademliaConfiguration i = KademliaConfiguration {
      kademliaPort :: Word16
    , kademliaId :: i
    }

-- | Discovery peers using the Kademlia DHT. Nodes in this network will store
--   their (assumed to be TCP transport) 'EndPointAddress'es and send them
--   over the wire on request. NB there are two notions of ID here: the
--   Kademlia IDs, and the 'EndPointAddress'es which are indexed by the former.
kademliaDiscovery
    :: forall m i .
       ( MonadIO m, Binary i, Ord i )
    => KademliaConfiguration i
    -> K.Node i
    -- ^ A known peer, necessary in order to join the network.
    -> EndPointAddress
    -- ^ My address. Will store it in the DHT.
    -> m (NetworkDiscovery KademliaDiscoveryErrorCode m)
kademliaDiscovery configuration initialPeer myAddress = do
    let kid :: KSerialize i
        kid = KSerialize (kademliaId configuration)
    let port :: Int
        port = fromIntegral (kademliaPort configuration)
    kademliaInst :: K.KademliaInstance (KSerialize i) (KSerialize EndPointAddress)
        <- liftIO $ K.create port kid
    peersTVar :: TVar.TVar (M.Map (K.Node (KSerialize i)) EndPointAddress)
        <- liftIO . TVar.newTVarIO $ M.empty
    let knownPeers = fmap (S.fromList . M.elems) . liftIO . TVar.readTVarIO $ peersTVar
    let discoverPeers = liftIO $ kademliaDiscoverPeers kademliaInst peersTVar
    let close = liftIO $ K.close kademliaInst
    liftIO $ kademliaJoinAndUpdate kademliaInst peersTVar initialPeer
    () <- liftIO $ K.store kademliaInst kid (KSerialize myAddress)
    pure $ NetworkDiscovery knownPeers discoverPeers close


kademliaDiscoverPeers
    :: forall i .
       ( Binary i, Ord i )
    => K.KademliaInstance (KSerialize i) (KSerialize EndPointAddress)
    -> TVar.TVar (M.Map (K.Node (KSerialize i)) EndPointAddress)
    -> IO (Either (DiscoveryError KademliaDiscoveryErrorCode) (S.Set EndPointAddress))
kademliaDiscoverPeers kademliaInst peersTVar = do
    recordedPeers <- TVar.readTVarIO peersTVar
    currentPeers <- K.dumpPeers kademliaInst
    -- The idea is to always update the TVar to the set of nodes in allPeers,
    -- but only lookup the addresses for nodes which are not in the recorded
    -- set to begin with.
    currentWithAddresses <- fmap (M.mapMaybe id) (kademliaLookupEndPointAddresses kademliaInst recordedPeers currentPeers)
    STM.atomically $ TVar.writeTVar peersTVar currentWithAddresses
    let new = currentWithAddresses `M.difference` recordedPeers
    pure $ Right (S.fromList (M.elems new))

kademliaJoinAndUpdate
    :: forall i .
       ( Binary i, Ord i )
    => K.KademliaInstance (KSerialize i) (KSerialize EndPointAddress)
    -> TVar.TVar (M.Map (K.Node (KSerialize i)) EndPointAddress)
    -> K.Node i
    -> IO (Either (DiscoveryError KademliaDiscoveryErrorCode) (S.Set EndPointAddress))
kademliaJoinAndUpdate kademliaInst peersTVar initialPeer = do
    result <- K.joinNetwork kademliaInst initialPeer'
    case result of
        K.IDClash -> pure $ Left (DiscoveryError KademliaIdClash "ID clash in network")
        K.NodeDown -> pure $ Left (DiscoveryError KademliaInitialPeerDown "Initial peer is down")
        -- [sic]
        K.JoinSucces -> do
            peerList <- K.dumpPeers kademliaInst
            -- We have the peers, but we do not have the 'EndPointAddress'es for
            -- them. We must ask the network for them.
            endPointAddresses <- fmap (M.mapMaybe id) (kademliaLookupEndPointAddresses kademliaInst M.empty peerList)
            STM.atomically $ TVar.writeTVar peersTVar endPointAddresses
            pure $ Right (S.fromList (M.elems endPointAddresses))
    where
    initialPeer' :: K.Node (KSerialize i)
    initialPeer' = case initialPeer of
        K.Node peer id -> K.Node peer (KSerialize id)

kademliaLookupEndPointAddresses
    :: forall i .
       ( Binary i, Ord i )
    => K.KademliaInstance (KSerialize i) (KSerialize EndPointAddress)
    -> M.Map (K.Node (KSerialize i)) EndPointAddress
    -> [K.Node (KSerialize i)]
    -> IO (M.Map (K.Node (KSerialize i)) (Maybe EndPointAddress))
kademliaLookupEndPointAddresses kademliaInst recordedPeers currentPeers = do
    -- TODO do this in parallel, as each one may induce a blocking lookup.
    endPointAddresses <- forM currentPeers (kademliaLookupEndPointAddress kademliaInst recordedPeers)
    let assoc :: [(K.Node (KSerialize i), Maybe EndPointAddress)]
        assoc = zip currentPeers endPointAddresses
    pure $ M.fromList assoc
  where
  isJust (Just _) = True
  isJust _ = False

kademliaLookupEndPointAddress
    :: forall i .
       ( Binary i, Ord i )
    => K.KademliaInstance (KSerialize i) (KSerialize EndPointAddress)
    -> M.Map (K.Node (KSerialize i)) EndPointAddress
    -> K.Node (KSerialize i)
    -> IO (Maybe EndPointAddress)
kademliaLookupEndPointAddress kademliaInst recordedPeers peer@(K.Node _ id) =
    case M.lookup peer recordedPeers of
        Nothing -> do
            outcome <- K.lookup kademliaInst id
            pure $ case outcome of
                Nothing -> Nothing
                Just (KSerialize endPointAddress, _) -> Just endPointAddress
        Just address -> pure (Just address)

data KademliaDiscoveryErrorCode
    = KademliaIdClash
    | KademliaInitialPeerDown
    deriving (Show, Typeable, Generic)
