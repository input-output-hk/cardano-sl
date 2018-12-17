{-# LANGUAGE OverloadedStrings #-}
module Test.Pos.Core.Bi
       ( tests
       ) where

import           Universum

import           Crypto.Hash (Blake2b_224)
import           Data.Fixed (Fixed (..))
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import           Data.Time.Units (fromMicroseconds)
import           Data.Typeable (typeRep)
import           Hedgehog (Gen, Property)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen

import           Cardano.Crypto.Wallet (xpub)
import           Pos.Binary.Class (Bi, Case (..), Raw (..), SizeOverride (..),
                     szCases)
import           Pos.Core.Attributes (Attributes, mkAttributes)
import           Pos.Core.Common (AddrAttributes (..), AddrSpendingData (..),
                     AddrStakeDistribution (..), AddrType (..),
                     BlockCount (..), ChainDifficulty (..), Coeff (..),
                     Coin (..), CoinPortion (..), ScriptVersion,
                     SharedSeed (..), StakeholderId, TxFeePolicy (..),
                     TxSizeLinear (..))
import           Pos.Core.Merkle (mkMerkleTree, mtRoot)
import           Pos.Core.Slotting (EpochIndex (..), EpochOrSlot (..),
                     FlatSlotId, LocalSlotIndex (..), SlotCount (..),
                     TimeDiff (..), Timestamp (..))
import           Pos.Crypto (AbstractHash (..), Hash, PublicKey (..),
                     abstractHash, redeemDeterministicKeyGen)


import           Test.Pos.Binary.Helpers (SizeTestConfig (..), scfg, sizeTest)
import           Test.Pos.Binary.Helpers.GoldenRoundTrip (goldenTestBi,
                     roundTripsBiBuildable, roundTripsBiShow)
import           Test.Pos.Core.ExampleHelpers (exampleAddrSpendingData_PubKey,
                     exampleAddress, exampleAddress1, exampleAddress2,
                     exampleAddress3, exampleAddress4, exampleEpochIndex,
                     examplePublicKey, exampleScript, exampleSlotId,
                     exampleSlotLeaders, exampleStakeholderId,
                     exampleStakesList, feedEpochSlots)
import           Test.Pos.Core.Gen
import           Test.Pos.Crypto.Bi (getBytes)
import           Test.Pos.Util.Golden (discoverGolden, eachOf)
import           Test.Pos.Util.Tripping (discoverRoundTrip)



--------------------------------------------------------------------------------
-- Address
--------------------------------------------------------------------------------
golden_Address :: Property
golden_Address = goldenTestBi exampleAddress "test/golden/bi/Address0"

golden_Address1 :: Property
golden_Address1 = goldenTestBi exampleAddress1 "test/golden/bi/Address1"

golden_Address2 :: Property
golden_Address2 = goldenTestBi exampleAddress2 "test/golden/bi/Address2"

golden_Address3 :: Property
golden_Address3 = goldenTestBi exampleAddress3 "test/golden/bi/Address3"

golden_Address4 :: Property
golden_Address4 = goldenTestBi exampleAddress4 "test/golden/bi/Address4"

roundTripAddressBi :: Property
roundTripAddressBi = eachOf 1000 genAddress roundTripsBiBuildable

--------------------------------------------------------------------------------
-- AddrSpendingData
--------------------------------------------------------------------------------
golden_AddrSpendingData_PubKey :: Property
golden_AddrSpendingData_PubKey = goldenTestBi exampleAddrSpendingData_PubKey
                                              "test/golden/AddrSpendingData_PubKey"

golden_AddrSpendingData_Script :: Property
golden_AddrSpendingData_Script = goldenTestBi asd "test/golden/AddrSpendingData_Script"
  where asd = ScriptASD exampleScript

golden_AddrSpendingData_Redeem :: Property
golden_AddrSpendingData_Redeem = goldenTestBi asd "test/golden/AddrSpendingData_Redeem"
  where
    asd = RedeemASD redeemPublicKey
    Just redeemPublicKey = fst <$> redeemDeterministicKeyGen (getBytes 0 32)

golden_AddrSpendingData_Unknown :: Property
golden_AddrSpendingData_Unknown = goldenTestBi asd "test/golden/AddrSpendingData_Unknown"
  where asd = UnknownASD 247 (getBytes 3 32)

roundTripAddrSpendingDataBi :: Property
roundTripAddrSpendingDataBi = eachOf 1000 genAddrSpendingData roundTripsBiBuildable

--------------------------------------------------------------------------------
-- AddrStakeDistribution
--------------------------------------------------------------------------------
golden_AddrStakeDistribution_Bootstrap :: Property
golden_AddrStakeDistribution_Bootstrap =
    goldenTestBi BootstrapEraDistr "test/golden/AddrStakeDistribution_Bootstrap"

golden_AddrStakeDistribution_SingleKey :: Property
golden_AddrStakeDistribution_SingleKey =
    goldenTestBi asd "test/golden/AddrStakeDistribution_SingleKey"
  where
    asd = SingleKeyDistr (abstractHash examplePublicKey)

golden_AddrStakeDistribution_UnsafeMultiKey :: Property
golden_AddrStakeDistribution_UnsafeMultiKey =
    goldenTestBi asd "test/golden/AddrStakeDistribution_UnsafeMultiKey"
  where
    asd   =  M.fromList (zip sis coins) :: Map StakeholderId CoinPortion
    sis   = [si1, si2, si3]
    coins = map (CoinPortion . exp10_14) [3,2,5]
    exp10_14 x = x * (10 :: Word64) ^ (14 :: Word64)
    Right si1 = abstractHash . PublicKey <$> xpub (getBytes  0 64)
    Right si2 = abstractHash . PublicKey <$> xpub (getBytes 13 64)
    Right si3 = abstractHash . PublicKey <$> xpub (getBytes 27 64)

roundTripAddrStakeDistributionBi :: Property
roundTripAddrStakeDistributionBi = eachOf 1000 genAddrStakeDistribution roundTripsBiBuildable

--------------------------------------------------------------------------------
-- AddrType
--------------------------------------------------------------------------------
golden_AddrType_PK :: Property
golden_AddrType_PK = goldenTestBi ATPubKey "test/golden/AddrType_PK"

golden_AddrType_S :: Property
golden_AddrType_S = goldenTestBi ATScript "test/golden/AddrType_S"

golden_AddrType_R :: Property
golden_AddrType_R = goldenTestBi ATRedeem "test/golden/AddrType_R"

golden_AddrType_U :: Property
golden_AddrType_U = goldenTestBi (ATUnknown 57) "test/golden/AddrType_U"

roundTripAddrTypeBi :: Property
roundTripAddrTypeBi = eachOf 1000 genAddrType roundTripsBiShow

--------------------------------------------------------------------------------
-- BlockCount
--------------------------------------------------------------------------------
golden_BlockCount :: Property
golden_BlockCount = goldenTestBi bc "test/golden/BlockCount"
  where bc = BlockCount 999

roundTripBlockCountBi :: Property
roundTripBlockCountBi = eachOf 1000 genBlockCount roundTripsBiBuildable

--------------------------------------------------------------------------------
-- ChainDifficulty
--------------------------------------------------------------------------------
golden_ChainDifficulty :: Property
golden_ChainDifficulty = goldenTestBi cd "test/golden/ChainDifficulty"
  where cd = ChainDifficulty (BlockCount 9999)

roundTripChainDifficultyBi :: Property
roundTripChainDifficultyBi = eachOf 1000 genChainDifficulty roundTripsBiBuildable

--------------------------------------------------------------------------------
-- Coeff
--------------------------------------------------------------------------------
golden_Coeff :: Property
golden_Coeff = goldenTestBi c "test/golden/Coeff"
  where c = Coeff (MkFixed 101)

roundTripCoeffBi :: Property
roundTripCoeffBi = eachOf 1000 genCoeff roundTripsBiBuildable

--------------------------------------------------------------------------------
-- Coin
--------------------------------------------------------------------------------
golden_Coin :: Property
golden_Coin = goldenTestBi c "test/golden/Coin"
  where c = Coin 9732

roundTripCoinBi :: Property
roundTripCoinBi = eachOf 1000 genCoin roundTripsBiBuildable

--------------------------------------------------------------------------------
-- CoinPortion
--------------------------------------------------------------------------------
golden_CoinPortion :: Property
golden_CoinPortion = goldenTestBi c "test/golden/CoinPortion"
  where c = CoinPortion 9702

roundTripCoinPortionBi :: Property
roundTripCoinPortionBi = eachOf 1000 genCoinPortion roundTripsBiBuildable

--------------------------------------------------------------------------------
-- Script
--------------------------------------------------------------------------------
golden_Script :: Property
golden_Script = goldenTestBi exampleScript "test/golden/Script"

roundTripScriptBi :: Property
roundTripScriptBi = eachOf 1000 genScript roundTripsBiBuildable

--------------------------------------------------------------------------------
-- ScriptVersion
--------------------------------------------------------------------------------
golden_ScriptVersion :: Property
golden_ScriptVersion = goldenTestBi sv "test/golden/ScriptVersion"
  where sv = 6001 :: ScriptVersion

roundTripScriptVersionBi :: Property
roundTripScriptVersionBi = eachOf 1000 genScriptVersion roundTripsBiBuildable

--------------------------------------------------------------------------------
-- SharedSeed
--------------------------------------------------------------------------------
golden_SharedSeed :: Property
golden_SharedSeed = goldenTestBi s "test/golden/SharedSeed"
  where s = SharedSeed (getBytes 8 32)

roundTripSharedSeedBi :: Property
roundTripSharedSeedBi = eachOf 1000 genSharedSeed roundTripsBiBuildable

--------------------------------------------------------------------------------
-- SlotLeaders
--------------------------------------------------------------------------------
golden_SlotLeaders :: Property
golden_SlotLeaders = goldenTestBi exampleSlotLeaders "test/golden/SlotLeaders"

roundTripSlotLeadersBi :: Property
roundTripSlotLeadersBi = eachOf 1000 genSlotLeaders roundTripsBiShow

--------------------------------------------------------------------------------
-- StakeholderId
--------------------------------------------------------------------------------
golden_StakeholderId :: Property
golden_StakeholderId =
    goldenTestBi exampleStakeholderId "test/golden/StakeholderId"

roundTripStakeholderIdBi :: Property
roundTripStakeholderIdBi = eachOf 1000 genStakeholderId roundTripsBiBuildable

--------------------------------------------------------------------------------
-- StakesList
--------------------------------------------------------------------------------
golden_StakesList :: Property
golden_StakesList = goldenTestBi exampleStakesList "test/golden/StakesList"

roundTripStakesListBi :: Property
roundTripStakesListBi = eachOf 1000 genStakesList roundTripsBiShow

--------------------------------------------------------------------------------
-- StakesMap
--------------------------------------------------------------------------------
golden_StakesMap :: Property
golden_StakesMap = goldenTestBi sm "test/golden/StakesMap"
  where sm = HM.fromList exampleStakesList

roundTripStakesMapBi :: Property
roundTripStakesMapBi = eachOf 1000 genStakesMap roundTripsBiShow

--------------------------------------------------------------------------------
-- TxFeePolicy
--------------------------------------------------------------------------------
golden_TxFeePolicy_Linear :: Property
golden_TxFeePolicy_Linear = goldenTestBi tfp "test/golden/TxFeePolicy_Linear"
  where
    tfp = TxFeePolicyTxSizeLinear (TxSizeLinear c1 c2)
    c1 = Coeff (MkFixed 99)
    c2 = Coeff (MkFixed 777)

golden_TxFeePolicy_Unknown :: Property
golden_TxFeePolicy_Unknown = goldenTestBi tfp "test/golden/TxFeePolicy_Unknown"
  where
    tfp = TxFeePolicyUnknown 101 (getBytes 40 32)

roundTripTxFeePolicyBi :: Property
roundTripTxFeePolicyBi = eachOf 1000 genTxFeePolicy roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxSizeLinear
--------------------------------------------------------------------------------
golden_TxSizeLinear :: Property
golden_TxSizeLinear = goldenTestBi tsl "test/golden/TxSizeLinear"
  where
    tsl = TxSizeLinear c1 c2
    c1 = Coeff (MkFixed 999)
    c2 = Coeff (MkFixed 77)

roundTripTxSizeLinearBi :: Property
roundTripTxSizeLinearBi = eachOf 1000 genTxSizeLinear roundTripsBiBuildable

--------------------------------------------------------------------------------
-- EpochIndex
--------------------------------------------------------------------------------
golden_EpochIndex :: Property
golden_EpochIndex = goldenTestBi exampleEpochIndex "test/golden/EpochIndex"

roundTripEpochIndexBi :: Property
roundTripEpochIndexBi = eachOf 1000 genEpochIndex roundTripsBiBuildable

--------------------------------------------------------------------------------
-- EpochOrSlot
--------------------------------------------------------------------------------
golden_EpochOrSlotEI :: Property
golden_EpochOrSlotEI = goldenTestBi eos "test/golden/EpochOrSlotEI"
  where eos = EpochOrSlot (Left (EpochIndex 14))

golden_EpochOrSlotSI :: Property
golden_EpochOrSlotSI = goldenTestBi eos "test/golden/EpochOrSlotSI"
  where eos = EpochOrSlot (Right exampleSlotId)

roundTripEpochOrSlotBi :: Property
roundTripEpochOrSlotBi = eachOf 1000 (feedEpochSlots genEpochOrSlot) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- FlatSlotId
--------------------------------------------------------------------------------
golden_FlatSlotId :: Property
golden_FlatSlotId = goldenTestBi fsi "test/golden/FlatSlotId"
  where fsi = 5001 :: FlatSlotId

roundTripFlatSlotIdBi :: Property
roundTripFlatSlotIdBi = eachOf 1000 genFlatSlotId roundTripsBiBuildable

--------------------------------------------------------------------------------
-- LocalSlotIndex
--------------------------------------------------------------------------------
golden_LocalSlotIndex :: Property
golden_LocalSlotIndex = goldenTestBi lsi "test/golden/LocalSlotIndex"
  where lsi = UnsafeLocalSlotIndex 52

roundTripLocalSlotIndexBi :: Property
roundTripLocalSlotIndexBi = eachOf 1000 (feedEpochSlots genLocalSlotIndex) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- SlotCount
--------------------------------------------------------------------------------
golden_SlotCount :: Property
golden_SlotCount = goldenTestBi sc "test/golden/SlotCount"
  where sc = SlotCount 474747

roundTripSlotCountBi :: Property
roundTripSlotCountBi = eachOf 1000 genSlotCount roundTripsBiBuildable

--------------------------------------------------------------------------------
-- SlotId
--------------------------------------------------------------------------------
golden_SlotId :: Property
golden_SlotId = goldenTestBi exampleSlotId "test/golden/SlotId"

roundTripSlotIdBi :: Property
roundTripSlotIdBi = eachOf 1000 (feedEpochSlots genSlotId) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TimeDiff
--------------------------------------------------------------------------------
golden_TimeDiff :: Property
golden_TimeDiff = goldenTestBi td "test/golden/TimeDiff"
  where td = TimeDiff 4747

roundTripTimeDiffBi :: Property
roundTripTimeDiffBi = eachOf 1000 genTimeDiff roundTripsBiBuildable

--------------------------------------------------------------------------------
-- Attributes
--------------------------------------------------------------------------------

golden_Attributes :: Property
golden_Attributes = goldenTestBi attrib "test/golden/Attributes"
    where attrib = mkAttributes ()

roundTripAttributes :: Property
roundTripAttributes = eachOf 50 (genAttributes (pure ())) roundTripsBiShow

--------------------------------------------------------------------------------
-- MerkleTree
--------------------------------------------------------------------------------

golden_MerkleTree :: Property
golden_MerkleTree = goldenTestBi mTree "test/golden/MerkleTree"
    where mTree = mkMerkleTree [(abstractHash $ Raw ("9") :: Hash Raw)]


roundTripMerkleTree :: Property
roundTripMerkleTree = eachOf 10 (genMerkleTree genHashRaw) roundTripsBiShow

--------------------------------------------------------------------------------
-- MerkleRoot
--------------------------------------------------------------------------------

golden_MerkleRoot :: Property
golden_MerkleRoot = goldenTestBi mTree "test/golden/MerkleRoot"
    where mTree = mtRoot $ mkMerkleTree [(abstractHash $ Raw ("9") :: Hash Raw)]

roundTripMerkleRoot :: Property
roundTripMerkleRoot = eachOf 10 (genMerkleRoot genHashRaw) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TimeStamp
--------------------------------------------------------------------------------

golden_Timestamp :: Property
golden_Timestamp = goldenTestBi timeStamp "test/golden/TimeStamp"
  where
    timeStamp = Timestamp $ fromMicroseconds 47

roundTripTimestamp :: Property
roundTripTimestamp = eachOf 50 genTimestamp roundTripsBiBuildable

sizeEstimates :: H.Group
sizeEstimates =
  let check :: forall a. (Show a, Bi a) => Gen a -> Property
      check g = sizeTest $ scfg { gen = g }
      pkOrRedeem (PubKeyASD _) = True
      pkOrRedeem (RedeemASD _) = True
      pkOrRedeem _             = False

      -- Explicit bounds for types, based on the generators from Gen.
      attrUnitSize = (typeRep (Proxy @(Attributes ()))
                     , SizeConstant 1)
      attrAddrSize = (typeRep (Proxy @(Attributes AddrAttributes)),
                      SizeConstant (szCases [ Case "min" 1, Case "max" 1024 ]))
      portionSize  = (typeRep (Proxy @(Map (AbstractHash Blake2b_224 PublicKey) CoinPortion)),
                      SizeConstant (szCases [ Case "min" 1, Case "max" 1024 ]))

  in H.Group "Encoded size bounds for core types."
        [ ("Coin"                 , check genCoin)
        , ("BlockCount"           , check genBlockCount)
        , ("Attributes ()"        , sizeTest $ scfg
              { gen = genAttributes (pure ())
              , addlCtx = M.fromList [ attrUnitSize ]
              })
        , ("Attributes AddrAttributes", sizeTest $ scfg
              { gen = genAttributes genAddrAttributes
              , addlCtx = M.fromList [ attrAddrSize ]
              })
        , ("Address"              , sizeTest $ scfg
              { gen = genAddress
              , addlCtx = M.fromList [ attrAddrSize ]
              })
        , ("AddrStakeDistribution", sizeTest $ scfg
              { gen = genAddrStakeDistribution
              , addlCtx = M.fromList [ portionSize ]
              })
        , ("AddrSpendingData"     , sizeTest $ scfg
              { gen = Gen.filter pkOrRedeem genAddrSpendingData
              , addlCtx = M.fromList
                  [ (typeRep (Proxy @AddrSpendingData),
                     SelectCases ["PubKeyASD", "RedeemASD"])
                  ] })
        , ("AddrType"             , check genAddrType)
        ]

-----------------------------------------------------------------------
-- Main test export
-----------------------------------------------------------------------

tests :: IO Bool
tests = and <$> sequence
    [ H.checkSequential $$discoverGolden
    , H.checkParallel $$discoverRoundTrip
    , H.checkParallel sizeEstimates
    ]
