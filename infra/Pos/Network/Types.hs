{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Network.Types
    ( NetworkConfig (..)
    , Topology(..)
    , StaticPeers(..)
    , SubscriptionWorker(..)
    , Valency
    , Bucket(..)
    , Fallbacks
    , topologyNodeType
    , topologySubscriberNodeType
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
import           Pos.Network.Yaml                      (NodeName (..))
import           Pos.Util.TimeWarp                     (addressToNodeId)
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

type Valency = Int

type Fallbacks = Int

-- | Statically configured peers
--
-- Although the peers are statically configured, this is nonetheless stateful
-- because we re-read the file on SIGHUP.
data StaticPeers = StaticPeers {
      -- | Register a handler to be invoked whenver the static peers change
      --
      -- The handler will also be called on registration
      -- (with the current value).
      staticPeersOnChange :: (Peers NodeId -> IO ()) -> IO ()
    }

instance Show StaticPeers where
  show _ = "<<StaticPeers>>"

-- | Topology of the network, from the point of view of the current node
data Topology kademlia =
    -- | All peers of the node have been statically configured
    --
    -- This is used for core and relay nodes
    TopologyCore !StaticPeers !(Maybe kademlia)

  | TopologyRelay !StaticPeers !(Maybe kademlia)

    -- | We discover our peers through DNS
    --
    -- This is used for behind-NAT nodes.
  | TopologyBehindNAT !(DnsDomains DNS.Domain)

    -- | We discover our peers through Kademlia
  | TopologyP2P !Valency !Fallbacks !kademlia

    -- | We discover our peers through Kademlia, and every node in the network
    -- is a core node.
  | TopologyTraditional !Valency !Fallbacks !kademlia

    -- | Light wallets simulate "real" edge nodes, but are configured with
    -- a static set of relays.
  | TopologyLightWallet ![NodeId]
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

data SubscriptionWorker kademlia =
    SubscriptionWorkerBehindNAT (DnsDomains DNS.Domain)
  | SubscriptionWorkerKademlia kademlia NodeType Valency Fallbacks

-- | What kind of subscription worker do we run?
topologySubscriptionWorker :: Topology kademlia -> Maybe (SubscriptionWorker kademlia)
topologySubscriptionWorker = go
  where
    go TopologyCore{}                     = Nothing
    go TopologyRelay{}                    = Nothing
    go (TopologyBehindNAT doms)           = Just $ SubscriptionWorkerBehindNAT doms
    go (TopologyP2P v f kademlia)         = Just $ SubscriptionWorkerKademlia kademlia NodeRelay v f
    go (TopologyTraditional v f kademlia) = Just $ SubscriptionWorkerKademlia kademlia NodeCore v f
    go TopologyLightWallet{}              = Nothing

-- | Should we register to the Kademlia network?
topologyRunKademlia :: Topology kademlia -> Maybe kademlia
topologyRunKademlia = go
  where
    go (TopologyCore  _        mKademlia) = mKademlia
    go (TopologyRelay _        mKademlia) = mKademlia
    go TopologyBehindNAT{}                = Nothing
    go (TopologyP2P _ _         kademlia) = Just kademlia
    go (TopologyTraditional _ _ kademlia) = Just kademlia
    go TopologyLightWallet{}              = Nothing

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
initQueue :: FormatMsg msg
          => NetworkConfig kademlia
          -> IO (OutboundQ msg NodeId Bucket)
initQueue NetworkConfig{..} = do
    oq <- OQ.new selfName enqueuePolicy dequeuePolicy failurePolicy

    case ncTopology of
      TopologyLightWallet peers -> do
        let peers' = simplePeers $ map (NodeRelay, ) peers
        OQ.updatePeersBucket oq BucketStatic (\_ -> peers')
      TopologyBehindNAT _ ->
        -- subscription worker is responsible for adding peers
        return ()
      TopologyP2P{} ->
        -- Kademlia worker is responsible for adding peers
        return ()
      TopologyTraditional{} ->
        -- Kademlia worker is responsible for adding peers
        return ()
      TopologyCore StaticPeers{..} _ ->
        staticPeersOnChange $ \peers ->
          OQ.updatePeersBucket oq BucketStatic (\_ -> peers)
      TopologyRelay StaticPeers{..} _ ->
        staticPeersOnChange $ \peers ->
          OQ.updatePeersBucket oq BucketStatic (\_ -> peers)

    return oq
  where
    ourNodeType   = topologyNodeType ncTopology
    selfName      = fromMaybe "self" ncSelfName
    enqueuePolicy = OQ.defaultEnqueuePolicy ourNodeType
    dequeuePolicy = OQ.defaultDequeuePolicy ourNodeType
    failurePolicy = OQ.defaultFailurePolicy ourNodeType

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
