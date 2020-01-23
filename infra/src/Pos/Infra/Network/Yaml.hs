{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Infrastructure for parsing the .yaml network topology file

module Pos.Infra.Network.Yaml
       (
         Topology(..)
       , AllStaticallyKnownPeers(..)
       , DnsDomains(..)
       , NodeRegion(..)
       , NodeRoutes(..)
       , NodeMetadata(..)
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
    toJSON (AllStaticallyKnownPeers pMap) = toJSON pMap

instance FromJSON AllStaticallyKnownPeers where
    parseJSON = A.withObject "AllStaticallyKnownPeers" $ \obj ->
        AllStaticallyKnownPeers . M.fromList <$> mapM aux (HM.toList obj)
      where
        aux :: (Text, A.Value) -> A.Parser (NodeName, NodeMetadata)
        aux (name, val) = (NodeName name, ) <$> parseJSON val

newtype NodeRegion = NodeRegion Text
    deriving (Show, Generic, Ord, Eq, IsString)

instance ToJSON NodeRegion where
    toJSON (NodeRegion text) = A.String text

instance FromJSON NodeRegion where
    parseJSON = fmap NodeRegion . parseJSON

newtype NodeRoutes = NodeRoutes [[NodeName]]
    deriving (Eq, Generic, Show)

instance ToJSON NodeRoutes where
    toJSON (NodeRoutes nameList) = toJSON nameList

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

      -- | Should the node be registered in the public DNS?
    , nmPublicDNS  :: !InPublicDNS

      -- | Maximum number of subscribers (only relevant for relays)
    , nmMaxSubscrs :: !OQ.MaxBucketSize
    }
    deriving (Eq, Generic, Show)

type InPublicDNS = Bool

----------------------------------------------------------------------------
-- JSON instances
----------------------------------------------------------------------------

instance ToJSON NodeMetadata where
    toJSON
        (NodeMetadata
            nmType
            nmRegion
            nmRoutes
            nmSubscribe
            nmValency
            nmFallbacks
            nmAddress
            nmPublicDNS
            nmMaxSubscrs) = do
                case nmAddress of
                    NodeAddrExact ip (Just port) ->
                        A.object [
                             "type" .= (toJSON nmType)
                           , "region" .= (toJSON nmRegion)
                           , "static-routes" .= (toJSON nmRoutes)
                           , "dynamic-subscribe" .= (toJSON nmSubscribe)
                           , "valency" .= (toJSON nmValency)
                           , "fallbacks" .= (toJSON nmFallbacks)
                           , "addr" .= (toJSON ip)
                           , "port" .= (toJSON port)
                           , "kademlia" .= (toJSON ())
                           , "public" .= (toJSON nmPublicDNS)
                           , "maxSubscrs" .= (toJSON nmMaxSubscrs)
                         ]
                    NodeAddrExact ip Nothing ->
                        A.object [
                            "type" .= (toJSON nmType)
                          , "region" .= (toJSON nmRegion)
                          , "static-routes" .= (toJSON nmRoutes)
                          , "dynamic-subscribe" .= (toJSON nmSubscribe)
                          , "valency" .= (toJSON nmValency)
                          , "fallbacks" .= (toJSON nmFallbacks)
                          , "addr" .= (toJSON ip)
                          , "port" .= (A.String "3000")
                          , "kademlia" .= (toJSON ())
                          , "public" .= (toJSON nmPublicDNS)
                          , "maxSubscrs" .= (toJSON nmMaxSubscrs)
                         ]
                    NodeAddrDNS (Just host) (Just port) -> do
                        let converted = decodeUtf8 @Text @ByteString host
                        A.object [
                            "type" .= (toJSON nmType)
                          , "region" .= (toJSON nmRegion)
                          , "static-routes" .= (toJSON nmRoutes)
                          , "dynamic-subscribe" .= (toJSON nmSubscribe)
                          , "valency" .= (toJSON nmValency)
                          , "fallbacks" .= (toJSON nmFallbacks)
                          , "host" .= (toJSON converted)
                          , "port" .= (toJSON port)
                          , "kademlia" .= (toJSON ())
                          , "public" .= (toJSON nmPublicDNS)
                          , "maxSubscrs" .= (toJSON nmMaxSubscrs)
                         ]
                    NodeAddrDNS (Just host) Nothing -> do
                        let converted = decodeUtf8 @Text @ByteString host
                        A.object [
                            "type" .= (toJSON nmType)
                          , "region" .= (toJSON nmRegion)
                          , "static-routes" .= (toJSON nmRoutes)
                          , "dynamic-subscribe" .= (toJSON nmSubscribe)
                          , "valency" .= (toJSON nmValency)
                          , "fallbacks" .= (toJSON nmFallbacks)
                          , "host" .= (toJSON converted)
                          , "port" .= (A.String "3000")
                          , "kademlia" .= (toJSON ())
                          , "public" .= (toJSON nmPublicDNS)
                          , "maxSubscrs" .= (toJSON nmMaxSubscrs)
                         ]
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
    toJSON (TopologyStatic (AllStaticallyKnownPeers pMap)) =
        A.object $ [ "nodes" .= (toJSON (AllStaticallyKnownPeers pMap) ) ]
    toJSON (TopologyBehindNAT
                   topologyValency
                   topologyFallbacks
                   topologyDnsDomains) =
                       A.object $ [ "wallet" .=
                           (A.object $ [ "relays" .= (toJSON topologyDnsDomains)
                                       , "valency" .= (toJSON topologyValency)
                                       , "fallbacks" .= (toJSON topologyFallbacks)
                                      ])
                                  ]
    toJSON (TopologyTraditional
                   topologyValency
                   topologyFallbacks
                   topologyMaxSubscrs) = do
                       case topologyMaxSubscrs of
                           OQ.BucketSizeMax int ->
                               A.object [ "p2p" .= (
                                   A.object [ "variant" .= A.String "traditional"
                                                , "valency" .= (toJSON topologyValency)
                                                , "fallbacks" .= (toJSON topologyFallbacks)
                                                , "maxSubscrs" .= (toJSON int)
                                                ] ) ]
                           OQ.BucketSizeUnlimited ->
                               A.object [ "p2p" .= (
                                   A.object [ "variant" .= A.String "traditional"
                                            , "valency" .= (toJSON topologyValency)
                                            , "fallbacks" .= (toJSON topologyFallbacks)
                                            , "maxSubscrs" .= A.Null
                                             ] ) ]
    toJSON (TopologyP2P
                   topologyValency
                   topologyFallbacks
                   topologyMaxSubscrs) = do
                       case topologyMaxSubscrs of
                           OQ.BucketSizeMax int ->
                               A.object [ "p2p" .= (
                                   A.object [ "variant" .= A.String "normal"
                                              , "valency" .= (toJSON topologyValency)
                                              , "fallbacks" .= (toJSON topologyFallbacks)
                                              , "maxSubscrs" .= (toJSON int)
                                              ]     ) ]
                           OQ.BucketSizeUnlimited ->
                               A.object [ "p2p" .= (
                                   A.object [ "variant" .= (A.String "normal")
                                            , "valency" .= (toJSON topologyValency)
                                            , "fallbacks" .= (toJSON topologyFallbacks)
                                            , "maxSubscrs" .= A.Null
                                            ]      ) ]

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
