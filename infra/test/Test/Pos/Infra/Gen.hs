{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Test.Pos.Infra.Gen
        (
        -- DHT Generators
          genDataMsg
        , genInvMsg
        , genMempoolMsg
        , genReqMsg
        , genResMsg
        , genDHTData
        , genDHTKey

        -- Slotting Generators
        , genEpochSlottingData
        , genSlottingData

        -- Pos.Infra.Communication Generators
        , genHandlerSpec

        -- Pos.Infra.Network Generators
        , genAllStaticallyKnownPeers
        , genDnsDomains
        , genDomain
        , genMaxBucketSize
        , genNodeAddr
        , genNodeAddrMaybe
        , genNodeMetadata
        , genNodeName
        , genNodeType
        , genNodeRoutes
        , genNodeRegion
        , genTopology
        ) where

import           Universum

import           Data.IP
import qualified Data.Map as DM
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Network.Kademlia.HashNodeId (genNonce, hashAddress)

import           Network.Broadcast.OutboundQueue (MaxBucketSize (..))
import           Network.Broadcast.OutboundQueue.Types (NodeType (..))
import qualified Network.DNS as DNS
import           Pos.Core (EpochIndex (..))
import           Pos.Crypto.Random (deterministic)
import           Pos.Infra.Communication.Types.Protocol (HandlerSpec (..))
import           Pos.Infra.Communication.Types.Relay (DataMsg (..), InvMsg (..),
                     MempoolMsg (..), ReqMsg (..), ResMsg (..))
import           Pos.Infra.DHT (DHTData (..), DHTKey (..))
import           Pos.Infra.Network.DnsDomains (DnsDomains (..), NodeAddr (..))
import           Pos.Infra.Network.Types (NodeName (..))
import           Pos.Infra.Network.Yaml (AllStaticallyKnownPeers (..),
                     NodeMetadata (..), NodeRegion (..), NodeRoutes (..),
                     Topology (..))
import           Pos.Infra.Slotting.Types (EpochSlottingData (..), SlottingData,
                     createSlottingDataUnsafe)

import           Test.Pos.Core.Gen (gen32Bytes, genTimeDiff, genWord16)
import           Test.Pos.Util.Gen (genMillisecond)

----------------------------------------------------------------------------
-- DHT Generators
----------------------------------------------------------------------------

genInvMsg :: Gen a -> Gen (InvMsg a)
genInvMsg genA = InvMsg <$> genA

genReqMsg :: Gen (Maybe a) -> Gen (ReqMsg a)
genReqMsg genMA = ReqMsg <$> genMA

genResMsg :: Gen a -> Gen (ResMsg a)
genResMsg genA = ResMsg <$> genA <*> Gen.bool

genMempoolMsg :: Gen (MempoolMsg a)
genMempoolMsg = pure MempoolMsg

genDataMsg :: Gen a -> Gen (DataMsg a)
genDataMsg genA = DataMsg <$> genA

genDHTKey :: Gen DHTKey
genDHTKey = pure $ DHTKey $ hashAddress $ deterministic "nonce" genNonce

genDHTData :: Gen DHTData
genDHTData = pure $ DHTData ()

----------------------------------------------------------------------------
-- Slotting Generators
----------------------------------------------------------------------------

genEpochSlottingData :: Gen EpochSlottingData
genEpochSlottingData = EpochSlottingData <$> genMillisecond <*> genTimeDiff

genSlottingData :: Gen SlottingData
genSlottingData =
    createSlottingDataUnsafe <$> genEpochIndexDataMap range
  where
    -- Constructing a SlottingData requires at least two epochs
    -- or else 'createSlottingDataUnsafe' will throw an error.
    range = Range.linear 2 100

genEpochIndexDataMap
    :: Range Word64
    -> Gen (Map EpochIndex EpochSlottingData)
genEpochIndexDataMap range =
    DM.fromList <$> genEpochIndexDataPairs range

genEpochIndexDataPair :: Word64 -> Gen (EpochIndex, EpochSlottingData)
genEpochIndexDataPair x = (EpochIndex x,) <$> genEpochSlottingData

genEpochIndexDataPairs
    :: Range Word64
    -> Gen [(EpochIndex, EpochSlottingData)]
genEpochIndexDataPairs range = do
    len <- Gen.integral range
    foldM
        (\xs i -> (: xs) <$> genEpochIndexDataPair i)
        []
        [0..len]

----------------------------------------------------------------------------
-- Pos.Infra.Communication Generators
----------------------------------------------------------------------------

genHandlerSpec :: Gen HandlerSpec
genHandlerSpec = Gen.choice [ ConvHandler <$> genWord16
                              -- 0 is reserved for ConvHandler tag.
                              -- See HandlerSpec Bi instance.
                            , UnknownHandler
                                  <$> Gen.word8 (Range.linear 1 255)
                                  <*> gen32Bytes
                            ]

----------------------------------------------------------------------------
-- Pos.Infra.Network Generators
----------------------------------------------------------------------------

genDomain :: Gen DNS.Domain
genDomain = Gen.utf8 (Range.linear 1 127) Gen.alpha

genDnsDomains :: Gen (DnsDomains DNS.Domain)
genDnsDomains = DnsDomains <$> Gen.list (Range.linear 1 10) singletonNA
  where
    singletonNA = Gen.list (Range.singleton 1) (genNodeAddr genDomain)

genMaxBucketSize :: Gen MaxBucketSize
genMaxBucketSize = Gen.choice
    [ pure BucketSizeUnlimited
    , BucketSizeMax <$> Gen.int (Range.linear 1 100)
    ]

genNodeAddr :: (Gen a) -> Gen (NodeAddr a)
genNodeAddr genA = Gen.choice
    [ NodeAddrExact <$> genIP <*> (Just <$> (Gen.word16 Range.linearBounded))
    , NodeAddrDNS <$> genA <*> (Just <$> (Gen.word16 Range.linearBounded))
    ]
  where
    genIP =
        Gen.choice
            [ IPv4 .toIPv4 <$> Gen.list
                                   (Range.singleton 4)
                                   (Gen.int (Range.linear 1 300))
            , IPv6 .toIPv6 <$> Gen.list
                                   (Range.singleton 8)
                                   (Gen.int (Range.linear 1 300))
            ]

-- | NodeAddrDNS constructor's serialization will error if no
-- hostname is given, therefore we only generate Just values for
-- Maybe DNS.Domain. See ToJSON instance of NodeAddr (Maybe DNS.Domain).
genNodeAddrMaybe :: Gen (NodeAddr (Maybe DNS.Domain))
genNodeAddrMaybe = genNodeAddr (Just <$> genDomain)

genNodeMetadata :: Gen NodeMetadata
genNodeMetadata = do
    nmType'       <- genNodeType
    nmRegion'     <- genNodeRegion
    nmValency'    <- Gen.int (Range.linear 1 10)
    nmFallbacks'  <- Gen.int (Range.linear 1 10)
    nmAddress'    <- genNodeAddr (Just <$> genDomain)
    nmKademlia'   <- Gen.bool
    nmPublicDNS'  <- Gen.bool
    nmMaxSubscrs' <- genMaxBucketSize
    choiceInt <- Gen.int8 (Range.linear 1 10)
    -- Below generates a NodeMetaData with either an empty DnsDomains or
    -- empty NodeRoutes. Either occurs ~50% of the time; see FromJSON
    -- NodeMetadata instance for more details.
    if (choiceInt <= 5)
        then do
            nmRoutes' <- genNodeRoutes
            pure $ NodeMetadata
                       nmType'
                       nmRegion'
                       nmRoutes'
                       (DnsDomains [])
                       nmValency'
                       nmFallbacks'
                       nmAddress'
                       nmKademlia'
                       nmPublicDNS'
                       nmMaxSubscrs'
        else do
            nmSubscribe' <- genDnsDomains
            pure $ NodeMetadata
                       nmType'
                       nmRegion'
                       (NodeRoutes [])
                       nmSubscribe'
                       nmValency'
                       nmFallbacks'
                       nmAddress'
                       nmKademlia'
                       nmPublicDNS'
                       nmMaxSubscrs'

genNodeName :: Gen NodeName
genNodeName = NodeName <$> Gen.text (Range.linear 1 10) Gen.alphaNum

genNodeType :: Gen NodeType
genNodeType = Gen.choice [ pure NodeCore
                         , pure NodeEdge
                         , pure NodeRelay
                         ]

genNodeRoutes :: Gen NodeRoutes
genNodeRoutes = NodeRoutes <$> Gen.list (Range.linear 1 10) singletonNN
  where
    singletonNN = Gen.list (Range.singleton 1) genNodeName

genNodeRegion :: Gen NodeRegion
genNodeRegion = NodeRegion <$> Gen.text (Range.linear 1 10) Gen.alphaNum

genAllStaticallyKnownPeers :: Gen AllStaticallyKnownPeers
genAllStaticallyKnownPeers =
    AllStaticallyKnownPeers <$> customMapGen genNodeName genNodeMetadata

genTopology :: Gen Topology
genTopology = Gen.choice [ TopologyStatic <$> genAllStaticallyKnownPeers
                         , TopologyBehindNAT
                               <$> (Gen.int (Range.linear 1 100))
                               <*> (Gen.int (Range.linear 1 100))
                               <*> genDnsDomains
                         , TopologyP2P
                               <$> (Gen.int (Range.linear 1 100))
                               <*> (Gen.int (Range.linear 1 100))
                               <*> genMaxBucketSize
                         , TopologyTraditional
                               <$> (Gen.int (Range.linear 1 100))
                               <*> (Gen.int (Range.linear 1 100))
                               <*> genMaxBucketSize
                         ]

customMapGen :: (Ord k) => Gen k -> Gen v -> Gen (Map k v)
customMapGen genK genV = DM.fromList <$> Gen.list range gen
  where
    gen = (,) <$> genK <*> genV
    range = Range.linear 0 10

