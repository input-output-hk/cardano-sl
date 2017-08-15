{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module Pos.Network.Types
    ( NetworkConfig (..)
    , Topology(..)
    , StaticPeers(..)
    , SubscriptionWorker(..)
    , Bucket(..)
    , topologyNodeType
    , topologySubscriberNodeType
    , topologyUnknownNodeType
    , topologySubscriptionWorker
    , topologyRunKademlia
    , resolveDnsDomains
    , defaultNetworkConfig
    , initQueue
      -- * Auxiliary
    , Resolver
    , initDnsOnUse
      -- * Re-exports
      -- ** from .DnsDomains
    , DnsDomains(..)
      -- ** from time-warp
    , NodeType (..)
    , MsgType (..)
    , Origin (..)
      -- ** from .Yaml
    , Valency
    , Fallbacks
      -- ** other
    , NodeId (..)
    ) where

import           Data.IP                               (IPv4)
import           Network.Broadcast.OutboundQueue       (OutboundQ)
import qualified Network.Broadcast.OutboundQueue       as OQ
import           Network.Broadcast.OutboundQueue.Types
import           Network.DNS                           (DNSError)
import qualified Network.DNS                           as DNS
import           Node.Internal                         (NodeId (..))
import           Pos.Network.DnsDomains                (DnsDomains (..))
import qualified Pos.Network.DnsDomains                as DnsDomains
import qualified Pos.Network.Policy                    as Policy
import           Pos.Network.Yaml                      (NodeName (..),
                                                        Valency, Fallbacks)
import           Pos.Util.TimeWarp                     (addressToNodeId)
import qualified System.Metrics                        as Monitoring
import           System.Wlog.CanLog                    (WithLogger)
import           Universum                             hiding (show)
import           GHC.Show                              (Show(..))

-- | Information about the network in which a node participates.
data NetworkConfig kademlia = NetworkConfig
    { ncTopology    :: !(Topology kademlia)
      -- ^ Network topology from the point of view of the current node
    , ncDefaultPort :: !Word16
      -- ^ Port number to use when translating IP addresses to NodeIds
    , ncSelfName    :: !(Maybe NodeName)
      -- ^ Our node name (if known)
    }
  deriving (Show)

defaultNetworkConfig :: Topology topology -> NetworkConfig topology
defaultNetworkConfig ncTopology = NetworkConfig {
      ncDefaultPort = 3000
    , ncSelfName    = Nothing
    , ..
    }

-- | Statically configured peers
--
-- Although the peers are statically configured, this is nonetheless stateful
-- because we re-read the file on SIGHUP.
data StaticPeers = forall m. (MonadIO m, WithLogger m) => StaticPeers {
      -- | Register a handler to be invoked whenever the static peers change
      --
      -- The handler will also be called on registration
      -- (with the current value).
      staticPeersOnChange :: (Peers NodeId -> m ()) -> IO ()
    }

instance Show StaticPeers where
  show _ = "<<StaticPeers>>"

-- | Topology of the network, from the point of view of the current node
data Topology kademlia =
    -- | All peers of the node have been statically configured
    --
    -- This is used for core and relay nodes
    TopologyCore {
        topologyStaticPeers :: !StaticPeers
      , topologyOptKademlia :: !(Maybe kademlia)
      }

  | TopologyRelay {
        topologyStaticPeers :: !StaticPeers
      , topologyOptKademlia :: !(Maybe kademlia)
      }

    -- | We discover our peers through DNS
    --
    -- This is used for behind-NAT nodes.
  | TopologyBehindNAT {
        topologyValency    :: !Valency
      , topologyFallbacks  :: !Fallbacks
      , topologyDnsDomains :: !(DnsDomains DNS.Domain)
      }

    -- | We discover our peers through Kademlia
  | TopologyP2P {
        topologyValency   :: !Valency
      , topologyFallbacks :: !Fallbacks
      , topologyKademlia  :: !kademlia
      }

    -- | We discover our peers through Kademlia, and every node in the network
    -- is a core node.
  | TopologyTraditional {
        topologyValency   :: !Valency
      , topologyFallbacks :: !Fallbacks
      , topologyKademlia  :: !kademlia
      }

    -- | Light wallets simulate "real" edge nodes, but are configured with
    -- a static set of relays.
  | TopologyLightWallet {
        topologyRelays :: ![NodeId]
      }
  deriving (Show)

-- | Derive node type from its topology
topologyNodeType :: Topology kademlia -> NodeType
topologyNodeType TopologyCore{}        = NodeCore
topologyNodeType TopologyRelay{}       = NodeRelay
topologyNodeType TopologyBehindNAT{}   = NodeEdge
topologyNodeType TopologyP2P{}         = NodeEdge
topologyNodeType TopologyTraditional{} = NodeCore
topologyNodeType TopologyLightWallet{} = NodeEdge

-- | The NodeType to assign to subscribers. Give Nothing if subscribtion
-- is not allowed for a node with this topology.
--
-- TODO: We allow corf nodes to run Kademlia, but we do not run the subscription
-- listener on them currently. We may wish to make that configurable.
topologySubscriberNodeType :: Topology kademlia -> Maybe NodeType
topologySubscriberNodeType TopologyCore{}        = Nothing
topologySubscriberNodeType TopologyRelay{}       = Just NodeEdge
topologySubscriberNodeType TopologyBehindNAT{}   = Nothing
topologySubscriberNodeType TopologyP2P{}         = Just NodeRelay
topologySubscriberNodeType TopologyTraditional{} = Just NodeCore
topologySubscriberNodeType TopologyLightWallet{} = Nothing

-- | Assumed type for unknown nodes
topologyUnknownNodeType :: Topology kademlia -> OQ.UnknownNodeType NodeId
topologyUnknownNodeType topology = OQ.UnknownNodeType $ go topology
  where
    go :: Topology kademlia -> NodeId -> NodeType
    go TopologyCore{..}      = const NodeEdge
    go TopologyRelay{..}     = const NodeEdge
    go TopologyTraditional{} = const NodeCore
    go TopologyP2P{}         = const NodeEdge
    go TopologyBehindNAT{}   = const NodeEdge
    go TopologyLightWallet{} = const NodeEdge

data SubscriptionWorker kademlia =
    SubscriptionWorkerBehindNAT (DnsDomains DNS.Domain) Valency Fallbacks
  | SubscriptionWorkerKademlia kademlia NodeType Valency Fallbacks

-- | What kind of subscription worker do we run?
topologySubscriptionWorker :: Topology kademlia -> Maybe (SubscriptionWorker kademlia)
topologySubscriptionWorker = go
  where
    go TopologyCore{}          = Nothing
    go TopologyRelay{}         = Nothing
    go TopologyBehindNAT{..}   = Just $ SubscriptionWorkerBehindNAT
                                          topologyDnsDomains
                                          topologyValency
                                          topologyFallbacks
    go TopologyP2P{..}         = Just $ SubscriptionWorkerKademlia
                                          topologyKademlia
                                          NodeRelay
                                          topologyValency
                                          topologyFallbacks
    go TopologyTraditional{..} = Just $ SubscriptionWorkerKademlia
                                          topologyKademlia
                                          NodeCore
                                          topologyValency
                                          topologyFallbacks
    go TopologyLightWallet{}   = Nothing

-- | Should we register to the Kademlia network? If so, is it essential that we
-- successfully join it (contact at least one existing peer)? Second component
-- is 'True' if yes.
topologyRunKademlia :: Topology kademlia -> Maybe (kademlia, Bool)
topologyRunKademlia = go
  where
    go TopologyCore{..}        = flip (,) False <$> topologyOptKademlia
    go TopologyRelay{..}       = flip (,) False <$> topologyOptKademlia
    go TopologyBehindNAT{}     = Nothing
    go TopologyP2P{..}         = Just (topologyKademlia, True)
    go TopologyTraditional{..} = Just (topologyKademlia, True)
    go TopologyLightWallet{}   = Nothing

-- | Enqueue policy for the given topology
topologyEnqueuePolicy :: Topology kademia -> OQ.EnqueuePolicy NodeId
topologyEnqueuePolicy = go
  where
    go TopologyCore{}        = Policy.defaultEnqueuePolicyCore
    go TopologyRelay{}       = Policy.defaultEnqueuePolicyRelay
    go TopologyBehindNAT{..} = Policy.defaultEnqueuePolicyEdgeBehindNat
    go TopologyP2P{}         = Policy.defaultEnqueuePolicyEdgeP2P
    go TopologyTraditional{} = Policy.defaultEnqueuePolicyCore
    go TopologyLightWallet{} = Policy.defaultEnqueuePolicyEdgeBehindNat

-- | Dequeue policy for the given topology
topologyDequeuePolicy :: Topology kademia -> OQ.DequeuePolicy
topologyDequeuePolicy = go
  where
    go TopologyCore{}        = Policy.defaultDequeuePolicyCore
    go TopologyRelay{}       = Policy.defaultDequeuePolicyRelay
    go TopologyBehindNAT{..} = Policy.defaultDequeuePolicyEdgeBehindNat
    go TopologyP2P{}         = Policy.defaultDequeuePolicyEdgeP2P
    go TopologyTraditional{} = Policy.defaultDequeuePolicyCore
    go TopologyLightWallet{} = Policy.defaultDequeuePolicyEdgeBehindNat

-- | Failure policy for the given topology
topologyFailurePolicy :: Topology kademia -> OQ.FailurePolicy NodeId
topologyFailurePolicy = Policy.defaultFailurePolicy . topologyNodeType

-- | Maximum bucket size
--
-- TODO: This is just a placeholder for now; we probably want to make this
-- value configurable in the @topology.yaml@ file?
topologyMaxBucketSize :: Topology kademia -> Bucket -> OQ.MaxBucketSize
topologyMaxBucketSize _ _ = OQ.BucketSizeUnlimited

-- | Variation on resolveDnsDomains that returns node IDs
resolveDnsDomains :: NetworkConfig kademlia
                  -> DnsDomains DNS.Domain
                  -> IO (Either [DNSError] [NodeId])
resolveDnsDomains NetworkConfig{..} dnsDomains =
    initDnsOnUse $ \resolve ->
      fmap (fmap addressToNodeId) <$> DnsDomains.resolveDnsDomains
                                        resolve
                                        ncDefaultPort
                                        dnsDomains

-- | The various buckets we use for the outbound queue
data Bucket =
    -- | Bucket for nodes we add statically
    BucketStatic

    -- | Bucket for nodes added by the behind-NAT worker
  | BucketBehindNatWorker

    -- | Bucket for nodes added by the Kademlia worker
  | BucketKademliaWorker

    -- | Bucket for nodes added by the subscription listener
  | BucketSubscriptionListener
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | Initialize the outbound queue based on the network configuration
--
-- We add all statically known peers to the queue, so that we know to send
-- messages to those peers. This is relevant only for core nodes and
-- light wallets. In the former case, those core nodes will in turn add this
-- core node to /their/ outbound queue because this node would equally be
-- a statically known peer; in the latter case, light wallets are not expected
-- to receive any messages so messages in the reverse direction don't matter.
--
-- For behind NAT nodes and Kademlia nodes (P2P or traditional) we start
-- (elsewhere) specialized workers that add peers to the queue and subscribe
-- to (some of) those peers.
initQueue :: (MonadIO m, WithLogger m, FormatMsg msg)
          => NetworkConfig kademlia
          -> Maybe Monitoring.Store -- ^ EKG store (if used)
          -> m (OutboundQ msg NodeId Bucket)
initQueue NetworkConfig{..} mStore = do
    oq <- OQ.new (maybe "self" toString ncSelfName)
                 (topologyEnqueuePolicy   ncTopology)
                 (topologyDequeuePolicy   ncTopology)
                 (topologyFailurePolicy   ncTopology)
                 (topologyMaxBucketSize   ncTopology)
                 (topologyUnknownNodeType ncTopology)

    case mStore of
      Nothing    -> return () -- EKG store not used
      Just store -> liftIO $ OQ.registerQueueMetrics oq store

    case ncTopology of
      TopologyLightWallet peers -> do
        let peers' = simplePeers $ map (NodeRelay, ) peers
        void $ OQ.updatePeersBucket oq BucketStatic (\_ -> peers')
      TopologyBehindNAT{} ->
        -- subscription worker is responsible for adding peers
        return ()
      TopologyP2P{} ->
        -- Kademlia worker is responsible for adding peers
        return ()
      TopologyTraditional{} ->
        -- Kademlia worker is responsible for adding peers
        return ()
      TopologyCore StaticPeers{..} _ -> liftIO $
        staticPeersOnChange $ \peers -> do
          OQ.clearRecentFailures oq
          void $ OQ.updatePeersBucket oq BucketStatic (\_ -> peers)
      TopologyRelay StaticPeers{..} _ -> liftIO $
        staticPeersOnChange $ \peers -> do
          OQ.clearRecentFailures oq
          void $ OQ.updatePeersBucket oq BucketStatic (\_ -> peers)

    return oq

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

type Resolver = DNS.Domain -> IO (Either DNSError [IPv4])

-- | Initialize the DNS library whenever it's used
--
-- This isn't great for performance but it means that we do not initialize it
-- when we need it; initializing it once only on demand is possible but requires
-- jumping through too many hoops.
--
-- TODO: Make it possible to change DNS config (esp for use on Windows).
initDnsOnUse :: (Resolver -> IO a) -> IO a
initDnsOnUse k = k $ \dom -> do
    resolvSeed <- DNS.makeResolvSeed DNS.defaultResolvConf
    DNS.withResolver resolvSeed $ \resolver ->
      DNS.lookupA resolver dom
