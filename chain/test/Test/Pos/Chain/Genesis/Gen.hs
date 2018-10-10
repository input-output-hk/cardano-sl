module Test.Pos.Chain.Genesis.Gen
       ( genGenesisHash
       , genFakeAvvmOptions
       , genGenesisAvvmBalances
       , genGenesisData
       , genGenesisDelegation
       , genGenesisInitializer
       , genGenesisProtocolConstants
       , genGenesisSpec
       , genTestnetBalanceOptions
       , genStaticConfig
       ) where

import           Universum

import           Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Pos.Chain.Genesis (FakeAvvmOptions (..),
                     GenesisAvvmBalances (..), GenesisData (..),
                     GenesisDelegation (..), GenesisHash (..),
                     GenesisInitializer (..), GenesisNonAvvmBalances (..),
                     GenesisProtocolConstants (..), GenesisSpec (..),
                     GenesisWStakeholders (..), StaticConfig (..),
                     TestnetBalanceOptions (..), mkGenesisDelegation,
                     mkGenesisSpec)
import           Pos.Core (TxFeePolicy (..))
import           Pos.Crypto (ProtocolMagic)

import           Test.Pos.Chain.Delegation.Gen (genProxySKHeavy)
import           Test.Pos.Chain.Ssc.Gen (genVssCertificatesMap)
import           Test.Pos.Chain.Update.Gen (genBlockVersionData,
                     genBlockVersionDataByTxFP)
import           Test.Pos.Core.Gen (genAddress, genCoin, genCoinPortion,
                     genHashRaw, genSharedSeed, genStakeholderId, genTextHash,
                     genTimestampRoundedToSecond, genTxSizeLinear,
                     genVssMaxTTL, genVssMinTTL, genWord16)
import           Test.Pos.Crypto.Gen (genRedeemPublicKey)
import           Test.Pos.Util.Gen (genHashMap)


genGenesisHash :: Gen GenesisHash
genGenesisHash = do
    th <- genTextHash
    pure (GenesisHash (coerce th))

genStaticConfig :: ProtocolMagic -> Gen StaticConfig
genStaticConfig pm =
    Gen.choice [ GCSrc
                     <$> Gen.string (Range.constant 10 25) Gen.alphaNum
                     <*> genHashRaw
               , GCSpec <$> genGenesisSpec pm
               ]

genFakeAvvmOptions :: Gen FakeAvvmOptions
genFakeAvvmOptions =
    FakeAvvmOptions
        <$> Gen.word Range.constantBounded
        <*> Gen.word64 Range.constantBounded

genGenesisData :: ProtocolMagic -> Gen GenesisData
genGenesisData pm =
    GenesisData
        <$> genGenesisWStakeholders
        <*> genGenesisDelegation pm
        <*> genTimestampRoundedToSecond
        <*> genVssCertificatesMap pm
        <*> genGenesisNonAvvmBalances
        <*> genBlockVersionDataByTxFP genLinearTxFP
        <*> genGenesisProtocolConstants pm
        <*> genGenesisAvvmBalances
        <*> genSharedSeed
  where
    -- @TxFeePolicy@s ToJSON instance crashes if we have a
    -- TxFeePolicyUnknown value.
    genLinearTxFP = TxFeePolicyTxSizeLinear <$> genTxSizeLinear

genGenesisWStakeholders :: Gen GenesisWStakeholders
genGenesisWStakeholders = do
    mapSize <- Gen.int $ Range.linear 1 10
    sids    <- Gen.list (Range.singleton mapSize) genStakeholderId
    w16s    <- Gen.list (Range.singleton mapSize) genWord16
    pure $ GenesisWStakeholders $ M.fromList $ zip sids w16s

genGenesisNonAvvmBalances :: Gen GenesisNonAvvmBalances
genGenesisNonAvvmBalances = do
    hmSize    <- Gen.int $ Range.linear 1 10
    addresses <- Gen.list (Range.singleton hmSize) genAddress
    coins     <- Gen.list (Range.singleton hmSize) genCoin
    pure $ GenesisNonAvvmBalances $ HM.fromList $ zip addresses coins

genGenesisDelegation :: ProtocolMagic -> Gen (GenesisDelegation)
genGenesisDelegation pm = do
    proxySKHeavyList <- Gen.list (Range.linear 1 10) $ genProxySKHeavy pm
    case (mkGenesisDelegation proxySKHeavyList) of
        Left _       -> genGenesisDelegation pm
        Right genDel -> pure genDel

genGenesisInitializer :: Gen GenesisInitializer
genGenesisInitializer =
    GenesisInitializer
        <$> genTestnetBalanceOptions
        <*> genFakeAvvmOptions
        <*> genCoinPortion
        <*> Gen.bool
        <*> Gen.integral (Range.constant 0 10)

genGenesisProtocolConstants :: ProtocolMagic -> Gen GenesisProtocolConstants
genGenesisProtocolConstants pm =
    GenesisProtocolConstants
        <$> Gen.int (Range.constant 0 100)
        <*> pure pm
        <*> genVssMaxTTL
        <*> genVssMinTTL

genGenesisSpec :: ProtocolMagic -> Gen GenesisSpec
genGenesisSpec pm = mkGenSpec >>=  either (error . toText) pure
    where
        mkGenSpec = mkGenesisSpec
                      <$> genGenesisAvvmBalances
                      <*> genSharedSeed
                      <*> genGenesisDelegation pm
                      <*> genBlockVersionData
                      <*> genGenesisProtocolConstants pm
                      <*> genGenesisInitializer

genTestnetBalanceOptions :: Gen TestnetBalanceOptions
genTestnetBalanceOptions =
    TestnetBalanceOptions
        <$> Gen.word Range.constantBounded
        <*> Gen.word Range.constantBounded
        <*> Gen.word64 Range.constantBounded
        <*> Gen.double (Range.constant 0 10)
        <*> Gen.bool

genGenesisAvvmBalances :: Gen GenesisAvvmBalances
genGenesisAvvmBalances = GenesisAvvmBalances
    <$> genHashMap (Range.linear 1 10) genRedeemPublicKey genCoin
