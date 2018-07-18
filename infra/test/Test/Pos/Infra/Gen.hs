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
        ) where

import           Universum

import qualified Data.Map as DM
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Network.Kademlia.HashNodeId (genNonce, hashAddress)

import           Pos.Core (EpochIndex (..))
import           Pos.Crypto.Random (deterministic)
import           Pos.Infra.Communication.Types.Protocol (HandlerSpec (..))
import           Pos.Infra.Communication.Types.Relay (DataMsg (..), InvMsg (..),
                     MempoolMsg (..), ReqMsg (..), ResMsg (..))
import           Pos.Infra.DHT (DHTData (..), DHTKey (..))
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
    range = Range.constant 2 100

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
                                  <$> Gen.word8 (Range.constant 1 255)
                                  <*> gen32Bytes
                            ]
