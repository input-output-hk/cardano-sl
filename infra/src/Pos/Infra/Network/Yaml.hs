{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Infrastructure for parsing the .yaml network topology file

module Pos.Infra.Network.Yaml
       (
         Topology(..)
       , AllStaticallyKnownPeers(..)
       , DnsDomains(..)
       , KademliaParams(..)
       , KademliaId(..)
       , KademliaAddress(..)
       , NodeRegion(..)
       , NodeRoutes(..)
       , NodeMetadata(..)
       , RunKademlia
       , Valency
       , Fallbacks

       , StaticPolicies(..)
       , StaticEnqueuePolicy -- opaque
       , StaticDequeuePolicy -- opaque
       , StaticFailurePolicy -- opaque
       , fromStaticPolicies
       ) where


import           Universum

import           Data.Aeson (FromJSON (..), ToJSON (..), (.!=), (.:), (.:?),
                     (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Encoding.Internal as A
import qualified Data.Aeson.Types as A
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map.Strict as M
import qualified Network.Broadcast.OutboundQueue as OQ
import           Network.Broadcast.OutboundQueue.Types
import qualified Network.DNS as DNS

import           Pos.Infra.Network.DnsDomains (DnsDomains (..), NodeAddr (..),
                     extractNodeAddr)
import           Pos.Infra.Network.Types (Fallbacks, NodeName (..), Valency)
import           Pos.Util.Util (aesonError, toAesonError)

-- | Description of the network topology in a Yaml file
--
-- This differs from 'Pos.Infra.Network.Types.Topology' because for static nodes this
-- describes the entire network topology (all statically known nodes), not just
-- the topology from the point of view of the current node.
data Topology =
    -- | TopologyStatic topologyAllPeers
    TopologyStatic !AllStaticallyKnownPeers

    -- | TopologyBehindNAT
    --       topologyValency
    --       topologyFallbacks
    --       topologyDnsDomains
  | TopologyBehindNAT
        !Valency
        !Fallbacks
        !(DnsDomains DNS.Domain)

    -- | TopologyP2P
    --       topologyValency
    --       topologyFallbacks
    --       topologyMaxSubscrs
  | TopologyP2P
        !Valency
        !Fallbacks
        !OQ.MaxBucketSize

    -- | TopologyTraditional
    --       topologyValency
    --       topologyFallbacks
    --       topologyMaxSubscrs
  | TopologyTraditional
        !Valency
        !Fallbacks
        !OQ.MaxBucketSize
  deriving (Eq, Generic, Show)

-- | All statically known peers in the newtork
newtype AllStaticallyKnownPeers = AllStaticallyKnownPeers
    { allStaticallyKnownPeers :: Map NodeName NodeMetadata
    } deriving (Eq, Generic, Show)

instance ToJSON AllStaticallyKnownPeers where
    toEncoding (AllStaticallyKnownPeers pMap) = toEncoding pMap

instance FromJSON AllStaticallyKnownPeers where
    parseJSON = A.withObject "AllStaticallyKnownPeers" $ \obj ->
        AllStaticallyKnownPeers . M.fromList <$> mapM aux (HM.toList obj)
      where
        aux :: (Text, A.Value) -> A.Parser (NodeName, NodeMetadata)
        aux (name, val) = (NodeName name, ) <$> parseJSON val

newtype NodeRegion = NodeRegion Text
    deriving (Show, Generic, Ord, Eq, IsString)

instance ToJSON NodeRegion where
    toEncoding (NodeRegion text) = A.text text

instance FromJSON NodeRegion where
    parseJSON = fmap NodeRegion . parseJSON

newtype NodeRoutes = NodeRoutes [[NodeName]]
    deriving (Eq, Generic, Show)

instance ToJSON NodeRoutes where
    toEncoding (NodeRoutes nameList) = A.list toEncoding nameList

instance FromJSON NodeRoutes where
    parseJSON = fmap NodeRoutes . parseJSON


data NodeMetadata = NodeMetadata
    { -- | Node type
      nmType       :: !NodeType

      -- | Region
    , nmRegion     :: !NodeRegion

      -- | Static peers of this node
    , nmRoutes     :: !NodeRoutes

      -- | Dynamic peers: DNS names that give the peers to subscribe to.
      -- Can use this as an alternative to static 'nmRoutes'.
    , nmSubscribe  :: !(DnsDomains DNS.Domain)
      -- | How many peers to subscribe to (with 'nmSubscribe')
    , nmValency    :: !Valency
      -- | How many fallback peers to use (with 'nmSubscribe')
    , nmFallbacks  :: !Fallbacks

      -- | Address for this node
    , nmAddress    :: !(NodeAddr (Maybe DNS.Domain))

      -- | Should the node register itself with the Kademlia network?
    , nmKademlia   :: !RunKademlia

      -- | Should the node br registered in the public DNS?
    , nmPublicDNS  :: !InPublicDNS

      -- | Maximum number of subscribers (only relevant for relays)
    , nmMaxSubscrs :: !OQ.MaxBucketSize
    }
    deriving (Eq, Generic, Show)

type RunKademlia = Bool
type InPublicDNS = Bool

-- | Parameters for Kademlia, in case P2P or traditional topology are used.
data KademliaParams = KademliaParams
    { kpId              :: !(Maybe KademliaId)
      -- ^ Kademlia identifier. Optional; one can be generated for you.
    , kpPeers           :: ![KademliaAddress]
      -- ^ Initial Kademlia peers, for joining the network.
    , kpAddress         :: !(Maybe KademliaAddress)
      -- ^ External Kadmelia address.
    , kpBind            :: !(Maybe KademliaAddress)
      -- ^ Address at which to bind the Kademlia socket.
      -- Shouldn't be necessary to have a separate bind and public address.
      -- The Kademlia instance in fact shouldn't even need to know its own
      -- address (should only be used to bind the socket). But due to the way
      -- that responses for FIND_NODES are serialized, Kademlia needs to know
      -- its own external address [TW-153]. The mainline 'kademlia' package
      -- doesn't suffer this problem.
    , kpExplicitInitial :: !(Maybe Bool)
    , kpDumpFile        :: !(Maybe FilePath)
    }
    deriving (Show)

instance FromJSON KademliaParams where
    parseJSON = A.withObject "KademliaParams" $ \obj -> do
        kpId <- obj .:? "identifier"
        kpPeers <- obj .: "peers"
        kpAddress <- obj .:? "externalAddress"
        kpBind <- obj .:? "address"
        kpExplicitInitial <- obj .:? "explicitInitial"
        kpDumpFile <- obj .:? "dumpFile"
        return KademliaParams {..}

instance ToJSON KademliaParams where
    toJSON KademliaParams {..} = A.object [
          "identifier"      .= kpId
        , "peers"           .= kpPeers
        , "externalAddress" .= kpAddress
        , "address"         .= kpBind
        , "explicitInitial" .= kpExplicitInitial
        , "dumpFile"        .= kpDumpFile
        ]

-- | A Kademlia identifier in text representation (probably base64-url encoded).
newtype KademliaId = KademliaId String
    deriving (Show)

instance FromJSON KademliaId where
    parseJSON = fmap KademliaId . parseJSON

instance ToJSON KademliaId where
    toJSON (KademliaId txt) = toJSON txt

data KademliaAddress = KademliaAddress
    { kaHost :: !String
    , kaPort :: !Word16
    } deriving (Show)

instance FromJSON KademliaAddress where
    parseJSON = A.withObject "KademliaAddress " $ \obj ->
        KademliaAddress <$> obj .: "host" <*> obj .: "port"

instance ToJSON KademliaAddress where
    toJSON KademliaAddress {..} = A.object [
          "host" .= kaHost
        , "port" .= kaPort
        ]

----------------------------------------------------------------------------
-- JSON instances
----------------------------------------------------------------------------

instance ToJSON NodeMetadata where
    toEncoding
        (NodeMetadata
            nmType
            nmRegion
            nmRoutes
            nmSubscribe
            nmValency
            nmFallbacks
            nmAddress
            nmKademlia
            nmPublicDNS
            nmMaxSubscrs) = do
                case nmAddress of
                    NodeAddrExact ip (Just port) ->
                        A.pairs $
                             A.pairStr "type" (toEncoding nmType)
                         <>  A.pairStr "region" (toEncoding nmRegion)
                         <>  A.pairStr "static-routes" (toEncoding nmRoutes)
                         <>  A.pairStr
                                "dynamic-subscribe" (toEncoding nmSubscribe)
                         <>  A.pairStr "valency" (toEncoding nmValency)
                         <>  A.pairStr "fallbacks" (toEncoding nmFallbacks)
                         <>  A.pairStr "addr" (toEncoding ip)
                         <>  A.pairStr "port" (toEncoding port)
                         <>  A.pairStr "kademlia" (toEncoding nmKademlia)
                         <>  A.pairStr "public" (toEncoding nmPublicDNS)
                         <>  A.pairStr "maxSubscrs" (toEncoding nmMaxSubscrs)
                    NodeAddrExact ip Nothing ->
                        A.pairs $
                            A.pairStr "type" (toEncoding nmType)
                         <> A.pairStr "region" (toEncoding nmRegion)
                         <> A.pairStr "static-routes" (toEncoding nmRoutes)
                         <> A.pairStr
                                "dynamic-subscribe" (toEncoding nmSubscribe)
                         <> A.pairStr "valency" (toEncoding nmValency)
                         <> A.pairStr "fallbacks" (toEncoding nmFallbacks)
                         <> A.pairStr "addr" (toEncoding ip)
                         <> A.pairStr "port" (A.string "3000")
                         <> A.pairStr "kademlia" (toEncoding nmKademlia)
                         <> A.pairStr "public" (toEncoding nmPublicDNS)
                         <> A.pairStr "maxSubscrs" (toEncoding nmMaxSubscrs)
                    NodeAddrDNS (Just host) (Just port) -> do
                        let converted = decodeUtf8 @Text @ByteString host
                        A.pairs $
                            A.pairStr "type" (toEncoding nmType)
                         <> A.pairStr "region" (toEncoding nmRegion)
                         <> A.pairStr "static-routes" (toEncoding nmRoutes)
                         <> A.pairStr
                                "dynamic-subscribe" (toEncoding nmSubscribe)
                         <> A.pairStr "valency" (toEncoding nmValency)
                         <> A.pairStr "fallbacks" (toEncoding nmFallbacks)
                         <> A.pairStr "host" (toEncoding converted)
                         <> A.pairStr "port" (toEncoding port)
                         <> A.pairStr "kademlia" (toEncoding nmKademlia)
                         <> A.pairStr "public" (toEncoding nmPublicDNS)
                         <> A.pairStr "maxSubscrs" (toEncoding nmMaxSubscrs)
                    NodeAddrDNS (Just host) Nothing -> do
                        let converted = decodeUtf8 @Text @ByteString host
                        A.pairs $
                            A.pairStr "type" (toEncoding nmType)
                         <> A.pairStr "region" (toEncoding nmRegion)
                         <> A.pairStr "static-routes" (toEncoding nmRoutes)
                         <> A.pairStr
                            "dynamic-subscribe" (toEncoding nmSubscribe)
                         <> A.pairStr "valency" (toEncoding nmValency)
                         <> A.pairStr "fallbacks" (toEncoding nmFallbacks)
                         <> A.pairStr "host" (toEncoding converted)
                         <> A.pairStr "port" (A.string "3000")
                         <> A.pairStr "kademlia" (toEncoding nmKademlia)
                         <> A.pairStr "public" (toEncoding nmPublicDNS)
                         <> A.pairStr "maxSubscrs" (toEncoding nmMaxSubscrs)
                    NodeAddrDNS Nothing _ ->
                        error "Please enter a hostname"

instance FromJSON NodeMetadata where
    parseJSON = A.withObject "NodeMetadata" $ \obj -> do
        nmType       <- obj .: "type"
        nmRegion     <- obj .: "region"
        nmRoutes     <- obj .:? "static-routes"     .!= NodeRoutes []
        nmSubscribe  <- obj .:? "dynamic-subscribe" .!= DnsDomains []
        nmValency    <- obj .:? "valency"   .!= 1
        nmFallbacks  <- obj .:? "fallbacks" .!= 1
        nmAddress    <- extractNodeAddr return obj
        nmKademlia   <- obj .:? "kademlia" .!= defaultRunKademlia nmType
        nmPublicDNS  <- obj .:? "public"   .!= defaultInPublicDNS nmType
        nmMaxSubscrs <- mBucketSize <$> obj .:? "maxSubscrs"
        case (nmRoutes, nmSubscribe) of
            (NodeRoutes [], DnsDomains []) ->
              aesonError "One of 'static-routes' or 'dynamic-subscribe' must be given"
            (NodeRoutes (_:_), DnsDomains (_:_)) ->
              aesonError "Only one of 'static-routes' or 'dynamic-subscribe' may be given"
            _ -> return ()
        return NodeMetadata{..}
     where
       defaultRunKademlia :: NodeType -> RunKademlia
       defaultRunKademlia NodeCore  = False
       defaultRunKademlia NodeRelay = True
       defaultRunKademlia NodeEdge  = False

       defaultInPublicDNS :: NodeType -> InPublicDNS
       defaultInPublicDNS NodeCore  = False
       defaultInPublicDNS NodeRelay = True
       defaultInPublicDNS NodeEdge  = False

instance FromJSON OQ.Precedence where
    parseJSON = A.withText "Precedence" $ \typ -> do
        toAesonError $ case toString typ of
          "lowest"   -> Right OQ.PLowest
          "low"      -> Right OQ.PLow
          "medium"   -> Right OQ.PMedium
          "high"     -> Right OQ.PHigh
          "highest"  -> Right OQ.PHighest
          _otherwise -> Left $ "Invalid Precedence" <> show typ

instance ToJSON Topology where
    toEncoding (TopologyStatic (AllStaticallyKnownPeers pMap)) =
        A.pairs $ A.pairStr "nodes" (toEncoding (AllStaticallyKnownPeers pMap) )
    toEncoding (TopologyBehindNAT
                   topologyValency
                   topologyFallbacks
                   topologyDnsDomains) =
                       A.pairs $ A.pairStr "wallet"
                           $ A.pairs $ A.pairStr "relays"
                                           (toEncoding topologyDnsDomains)
                                    <> A.pairStr "valency"
                                           (toEncoding topologyValency)
                                    <> A.pairStr "fallbacks"
                                           (toEncoding topologyFallbacks)
    toEncoding (TopologyTraditional
                   topologyValency
                   topologyFallbacks
                   topologyMaxSubscrs) = do
                       case topologyMaxSubscrs of
                           OQ.BucketSizeMax int ->
                               A.pairs $ A.pairStr "p2p"
                                   $ A.pairs $ A.pairStr "variant"
                                                   (A.text "traditional")
                                            <> A.pairStr "valency"
                                                   (toEncoding topologyValency)
                                            <> A.pairStr "fallbacks"
                                                   (toEncoding topologyFallbacks)
                                            <> A.pairStr "maxSubscrs"
                                                   (toEncoding int)
                           OQ.BucketSizeUnlimited ->
                               A.pairs $ A.pairStr "p2p"
                                   $ A.pairs $ A.pairStr "variant"
                                                   (A.text "traditional")
                                            <> A.pairStr "valency"
                                                   (toEncoding topologyValency)
                                            <> A.pairStr "fallbacks"
                                                   (toEncoding topologyFallbacks)
                                            <> A.pairStr "maxSubscrs"
                                                   A.null_
    toEncoding (TopologyP2P
                   topologyValency
                   topologyFallbacks
                   topologyMaxSubscrs) = do
                       case topologyMaxSubscrs of
                           OQ.BucketSizeMax int ->
                               A.pairs $ A.pairStr "p2p"
                                   $ A.pairs $ A.pairStr "variant"
                                                   (A.text "normal")
                                            <> A.pairStr "valency"
                                                   (toEncoding topologyValency)
                                            <> A.pairStr "fallbacks"
                                                   (toEncoding topologyFallbacks)
                                            <> A.pairStr "maxSubscrs"
                                                   (toEncoding int)
                           OQ.BucketSizeUnlimited ->
                               A.pairs $ A.pairStr "p2p"
                                   $ A.pairs $ A.pairStr "variant"
                                                   (A.text "normal")
                                            <> A.pairStr "valency"
                                                   (toEncoding topologyValency)
                                            <> A.pairStr "fallbacks"
                                                   (toEncoding topologyFallbacks)
                                            <> A.pairStr "maxSubscrs"
                                                   A.null_

instance FromJSON Topology where
    parseJSON = A.withObject "Topology" $ \obj -> do
        mNodes  <- obj .:? "nodes"
        mWallet <- obj .:? "wallet"
        mP2p    <- obj .:? "p2p"
        case (mNodes, mWallet, mP2p) of
          (Just nodes, Nothing, Nothing) ->
              TopologyStatic <$> parseJSON nodes
          (Nothing, Just wallet, Nothing) ->
              flip (A.withObject "wallet") wallet $ \walletObj -> do
                  topologyDnsDomains <- walletObj .:  "relays"
                  topologyValency    <- walletObj .:? "valency"   .!= 1
                  topologyFallbacks  <- walletObj .:? "fallbacks" .!= 1
                  return $ TopologyBehindNAT
                              topologyValency
                              topologyFallbacks
                              topologyDnsDomains
          (Nothing, Nothing, Just p2p) ->
              flip (A.withObject "P2P") p2p $ \p2pObj -> do
                  variantTxt         <- p2pObj .: "variant"
                  topologyValency    <- p2pObj .:? "valency"   .!= 3
                  topologyFallbacks  <- p2pObj .:? "fallbacks" .!= 1
                  topologyMaxSubscrs <- mBucketSize <$> p2pObj .:? "maxSubscrs"
                  flip (A.withText "P2P variant") variantTxt $ toAesonError . \case
                      "traditional" -> Right $ TopologyTraditional
                                                   topologyValency
                                                   topologyFallbacks
                                                   topologyMaxSubscrs
                      "normal"      -> Right $ TopologyP2P
                                                   topologyValency
                                                   topologyFallbacks
                                                   topologyMaxSubscrs
                      _             -> Left "P2P variant: expected \
                                            \'traditional' or 'normal'"
          _ ->
            aesonError "Topology: expected exactly one of 'nodes', 'wallet',\
                       \'relays', or 'p2p'"

mBucketSize :: Maybe Int -> OQ.MaxBucketSize
mBucketSize Nothing  = OQ.BucketSizeUnlimited
mBucketSize (Just n) = OQ.BucketSizeMax n

{-------------------------------------------------------------------------------
  Policies described in JSON/YAML.
-------------------------------------------------------------------------------}

-- | Policies described by a JSON/YAML.
data StaticPolicies = StaticPolicies
    { staticEnqueuePolicy :: StaticEnqueuePolicy
    , staticDequeuePolicy :: StaticDequeuePolicy
    , staticFailurePolicy :: StaticFailurePolicy
    }

-- | An enqueue policy which can be described by JSON/YAML.
newtype StaticEnqueuePolicy = StaticEnqueuePolicy
    { getStaticEnqueuePolicy :: forall nid. MsgType nid -> [OQ.Enqueue]
    }

-- | A dequeue policy which can be described by JSON/YAML.
newtype StaticDequeuePolicy = StaticDequeuePolicy
    { getStaticDequeuePolicy :: NodeType -> OQ.Dequeue
    }

newtype StaticFailurePolicy = StaticFailurePolicy
    { getStaticFailurePolicy :: forall nid. NodeType -> MsgType nid -> OQ.ReconsiderAfter
    }

fromStaticPolicies :: StaticPolicies
                   -> ( OQ.EnqueuePolicy nid
                      , OQ.DequeuePolicy
                      , OQ.FailurePolicy nid
                      )
fromStaticPolicies StaticPolicies {..} =
    ( fromStaticEnqueuePolicy staticEnqueuePolicy
    , fromStaticDequeuePolicy staticDequeuePolicy
    , fromStaticFailurePolicy staticFailurePolicy
    )

fromStaticEnqueuePolicy :: StaticEnqueuePolicy -> OQ.EnqueuePolicy nid
fromStaticEnqueuePolicy = getStaticEnqueuePolicy

fromStaticDequeuePolicy :: StaticDequeuePolicy -> OQ.DequeuePolicy
fromStaticDequeuePolicy = getStaticDequeuePolicy

fromStaticFailurePolicy :: StaticFailurePolicy -> OQ.FailurePolicy nid
fromStaticFailurePolicy p nodeType = const . getStaticFailurePolicy p nodeType

{-------------------------------------------------------------------------------
  Common patterns in the .yaml file
-------------------------------------------------------------------------------}

data SendOrForward t = SendOrForward
    { send    :: !t
    , forward :: !t
    }

newtype ByMsgType t = ByMsgType
    { byMsgType :: forall nid. MsgType nid -> t
    }

newtype ByNodeType t = ByNodeType
    { byNodeType :: NodeType -> t
    }

instance FromJSON t => FromJSON (SendOrForward t) where
    parseJSON = A.withObject "SendOrForward" $ \obj -> do
        send    <- obj .: "send"
        forward <- obj .: "forward"
        return SendOrForward {..}

instance FromJSON t => FromJSON (ByMsgType t) where
    parseJSON = A.withObject "ByMsgType" $ \obj -> do
        announceBlockHeader <- obj .: "announceBlockHeader"
        requestBlockHeaders <- obj .: "requestBlockHeaders"
        requestBlocks       <- obj .: "requestBlocks"
        transaction         <- obj .: "transaction"
        mpc                 <- obj .: "mpc"
        return $ ByMsgType $ \case
            MsgAnnounceBlockHeader _                 -> announceBlockHeader
            MsgRequestBlockHeaders _                 -> requestBlockHeaders
            MsgRequestBlocks       _                 -> requestBlocks
            MsgTransaction         OriginSender      -> send transaction
            MsgTransaction         (OriginForward _) -> forward transaction
            MsgMPC                 OriginSender      -> send mpc
            MsgMPC                 (OriginForward _) -> forward mpc

instance FromJSON t => FromJSON (ByNodeType t) where
    parseJSON = A.withObject "ByNodeType" $ \obj -> do
        core  <- obj .: "core"
        relay <- obj .: "relay"
        edge  <- obj .: "edge"
        return $ ByNodeType $ \case
            NodeCore  -> core
            NodeRelay -> relay
            NodeEdge  -> edge

----------------------------------------------------------------------------
-- FromJSON instances
----------------------------------------------------------------------------

instance FromJSON StaticPolicies where
    parseJSON = A.withObject "StaticPolicies" $ \obj -> do
        staticEnqueuePolicy <- obj .: "enqueue"
        staticDequeuePolicy <- obj .: "dequeue"
        staticFailurePolicy <- obj .: "failure"
        return StaticPolicies {..}

instance FromJSON StaticEnqueuePolicy where
    parseJSON = fmap aux . parseJSON
      where
        aux :: ByMsgType [OQ.Enqueue] -> StaticEnqueuePolicy
        aux f = StaticEnqueuePolicy (byMsgType f)

instance FromJSON StaticDequeuePolicy where
    parseJSON = fmap aux . parseJSON
      where
        aux :: ByNodeType OQ.Dequeue -> StaticDequeuePolicy
        aux f = StaticDequeuePolicy (byNodeType f)

instance FromJSON StaticFailurePolicy where
    parseJSON = fmap aux . parseJSON
      where
        aux :: ByNodeType (ByMsgType OQ.ReconsiderAfter) -> StaticFailurePolicy
        aux f = StaticFailurePolicy (byMsgType . byNodeType f)

----------------------------------------------------------------------------
-- Orphans
----------------------------------------------------------------------------

instance FromJSON OQ.Enqueue where
    parseJSON = A.withObject "Enqueue" $ \obj -> do
        mAll <- obj .:? "all"
        mOne <- obj .:? "one"
        case (mAll, mOne) of
            (Just all_, Nothing) -> parseEnqueueAll all_
            (Nothing, Just one_) -> parseEnqueueOne one_
            _                    -> aesonError "Enqueue: expected 'one' or 'all', and not both."
      where
        parseEnqueueAll :: A.Object -> A.Parser OQ.Enqueue
        parseEnqueueAll obj = do
            nodeType   <- obj .: "nodeType"
            maxAhead   <- obj .: "maxAhead"
            precedence <- obj .: "precedence"
            return $ OQ.EnqueueAll nodeType maxAhead precedence

        parseEnqueueOne :: A.Object -> A.Parser OQ.Enqueue
        parseEnqueueOne obj = do
            nodeTypes  <- obj .: "nodeTypes"
            maxAhead   <- obj .: "maxAhead"
            precedence <- obj .: "precedence"
            return $ OQ.EnqueueOne nodeTypes maxAhead precedence

instance FromJSON OQ.Dequeue where
    parseJSON = A.withObject "Dequeue" $ \obj -> do
        deqRateLimit   <- obj .:? "rateLimit"   .!= OQ.NoRateLimiting
        deqMaxInFlight <- obj .:  "maxInFlight"
        return OQ.Dequeue {..}

instance FromJSON OQ.MaxAhead where
    parseJSON = fmap OQ.MaxAhead . parseJSON

instance FromJSON OQ.MaxInFlight where
    parseJSON = fmap OQ.MaxInFlight . parseJSON

instance FromJSON OQ.RateLimit where
    parseJSON = fmap OQ.MaxMsgPerSec . parseJSON

instance FromJSON OQ.ReconsiderAfter where
    parseJSON = fmap (OQ.ReconsiderAfter . fromInteger) . parseJSON
