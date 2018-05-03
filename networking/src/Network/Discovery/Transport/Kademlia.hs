{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Discovery.Transport.Kademlia
       ( K.Node (..)
       , K.Peer (..)
       , KademliaConfiguration (..)
       , KademliaDiscoveryErrorCode (..)
       , kademliaDiscovery
       ) where

import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar
import           Control.Monad (forM)
import           Data.Binary (Binary, decodeOrFail, encode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Typeable (Typeable)
import           Data.Word (Word16)
import           GHC.Generics (Generic)
import qualified Network.Kademlia as K

import           Network.Discovery.Abstract
import           Network.Transport

-- | Wrapper which provides a 'K.Serialize' instance for any type with a
--   'Binary' instance.
newtype KSerialize i = KSerialize i
    deriving (Eq, Ord, Show)

instance Binary i => K.Serialize (KSerialize i) where
    fromBS bs = case decodeOrFail (BL.fromStrict bs) of
        Left (_, _, str)         -> Left str
        Right (unconsumed, _, i) -> Right (KSerialize i, BL.toStrict unconsumed)
    toBS (KSerialize i) = BL.toStrict . encode $ i

-- | Configuration for a Kademlia node.
data KademliaConfiguration i = KademliaConfiguration {
      kademliaBindAddress     :: (String, Word16)
    , kademliaExternalAddress :: (String, Word16)
    , kademliaId              :: i
      -- ^ Some value to use as the identifier for this node. To use it, it must
      --   have a 'Binary' instance. You may want to take a random value, and
      --   it should serialize to something long enough for your expected
      --   network size (every node in the network needs a unique id).
    }

-- | Discovery peers using the Kademlia DHT. Nodes in this network will store
--   their (assumed to be TCP transport) 'EndPointAddress'es and send them
--   over the wire on request. NB there are two notions of ID here: the
--   Kademlia IDs, and the 'EndPointAddress'es which are indexed by the former.
--
--   Many side-effects here: a Kademlia instance is created, grabbing a UDP
--   socket and using it to talk to a peer, storing data in the DHT once it has
--   been joined.
kademliaDiscovery
    :: forall i .
       (Binary i, Ord i, Show i)
    => KademliaConfiguration i
    -> K.Peer
    -- ^ A known peer, necessary in order to join the network.
    --   If there are no other peers in the network, use this node's id.
    -> EndPointAddress
    -- ^ Local endpoint address. Will store it in the DHT.
    -> IO (NetworkDiscovery KademliaDiscoveryErrorCode)
kademliaDiscovery configuration peer myAddress = do
    let kid :: KSerialize i
        kid = KSerialize (kademliaId configuration)
    -- A Kademlia instance to do the DHT magic.
    kademliaInst :: K.KademliaInstance (KSerialize i) (KSerialize EndPointAddress)
        <- K.create (kademliaBindAddress configuration)
                    (kademliaExternalAddress configuration) kid
    -- A TVar to cache the set of known peers at the last use of 'discoverPeers'
    peersTVar :: TVar.TVar (M.Map (K.Node (KSerialize i)) EndPointAddress)
        <- TVar.newTVarIO $ M.empty
    let knownPeers = fmap (S.fromList . M.elems) . TVar.readTVarIO $ peersTVar
    let discoverPeers = kademliaDiscoverPeers kademliaInst peersTVar
    let close = K.close kademliaInst
    -- Join the network and store the local 'EndPointAddress'.
    _ <- kademliaJoinAndUpdate kademliaInst peersTVar peer
    K.store kademliaInst kid (KSerialize myAddress)
    pure $ NetworkDiscovery knownPeers discoverPeers close

-- | Join a Kademlia network (using a given known node address) and update the
--   known peers cache.
kademliaJoinAndUpdate
    :: forall i .
       ( Binary i, Ord i )
    => K.KademliaInstance (KSerialize i) (KSerialize EndPointAddress)
    -> TVar.TVar (M.Map (K.Node (KSerialize i)) EndPointAddress)
    -> K.Peer
    -> IO (Either (DiscoveryError KademliaDiscoveryErrorCode) (S.Set EndPointAddress))
kademliaJoinAndUpdate kademliaInst peersTVar peer = do
    result <- K.joinNetwork kademliaInst peer
    case result of
        K.NodeBanned -> pure $ Left (DiscoveryError KademliaNodeBanned "Node is banned by network")
        K.IDClash -> pure $ Left (DiscoveryError KademliaIdClash "ID clash in network")
        K.NodeDown -> pure $ Left (DiscoveryError KademliaInitialPeerDown "Initial peer is down")
        -- [sic]
        K.JoinSuccess -> do
            peerList <- map fst <$> K.dumpPeers kademliaInst
            -- We have the peers, but we do not have the 'EndPointAddress'es for
            -- them. We must ask the network for them.
            endPointAddresses <- fmap (M.mapMaybe id) (kademliaLookupEndPointAddresses kademliaInst M.empty peerList)
            STM.atomically $ TVar.writeTVar peersTVar endPointAddresses
            pure $ Right (S.fromList (M.elems endPointAddresses))

-- | Update the known peers cache.
--
--   FIXME: error reporting. Should perhaps give a list of all of the errors
--   which occurred.
kademliaDiscoverPeers
    :: forall i .
       ( Binary i, Ord i )
    => K.KademliaInstance (KSerialize i) (KSerialize EndPointAddress)
    -> TVar.TVar (M.Map (K.Node (KSerialize i)) EndPointAddress)
    -> IO (Either (DiscoveryError KademliaDiscoveryErrorCode) (S.Set EndPointAddress))
kademliaDiscoverPeers kademliaInst peersTVar = do
    recordedPeers <- TVar.readTVarIO peersTVar
    currentPeers <- map fst <$> K.dumpPeers kademliaInst
    -- The idea is to always update the TVar to the set of nodes in allPeers,
    -- but only lookup the addresses for nodes which are not in the recorded
    -- set to begin with.
    currentWithAddresses <- fmap (M.mapMaybe id) (kademliaLookupEndPointAddresses kademliaInst recordedPeers currentPeers)
    STM.atomically $ TVar.writeTVar peersTVar currentWithAddresses
    let new = currentWithAddresses `M.difference` recordedPeers
    pure $ Right (S.fromList (M.elems new))

-- | Look up the 'EndPointAddress's for a set of nodes.
--   See 'kademliaLookupEndPointAddress'
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

-- | Look up the 'EndPointAddress' for a given node. The host and port of
--   the node are known, along with its Kademlia identifier, but the
--   'EndPointAddress' cannot be inferred from these things. The DHT stores
--   that 'EndPointAddress' using the node's Kademlia identifier as key, so
--   we look that up in the table. Nodes for which the 'EndPointAddress' is
--   already known are not looked up.
kademliaLookupEndPointAddress
    :: forall i .
       ( Binary i, Ord i )
    => K.KademliaInstance (KSerialize i) (KSerialize EndPointAddress)
    -> M.Map (K.Node (KSerialize i)) EndPointAddress
    -- ^ The current set of recorded peers. We don't lookup an 'EndPointAddress'
    --   for any of these, we just use the one in the map.
    -> K.Node (KSerialize i)
    -> IO (Maybe EndPointAddress)
kademliaLookupEndPointAddress kademliaInst recordedPeers peer@(K.Node _ nid) =
    case M.lookup peer recordedPeers of
        Nothing -> do
            outcome <- K.lookup kademliaInst nid
            pure $ case outcome of
                Nothing                              -> Nothing
                Just (KSerialize endPointAddress, _) -> Just endPointAddress
        Just address -> pure (Just address)

data KademliaDiscoveryErrorCode
    = KademliaIdClash
    | KademliaInitialPeerDown
    | KademliaNodeBanned
    deriving (Show, Typeable, Generic)
