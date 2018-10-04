module Test.Pos.Chain.Genesis.Gen
       ( genGenesisHash
       , genFakeAvvmOptions
       , genGenesisAvvmBalances
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

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Pos.Chain.Genesis (FakeAvvmOptions (..),
                     GenesisAvvmBalances (..), GenesisDelegation (..),
                     GenesisHash (..), GenesisInitializer (..),
                     GenesisProtocolConstants (..), GenesisSpec (..),
                     StaticConfig (..), TestnetBalanceOptions (..),
                     mkGenesisDelegation, mkGenesisSpec)
import           Pos.Crypto (ProtocolMagic)

import           Test.Pos.Chain.Delegation.Gen (genProxySKHeavy)
import           Test.Pos.Chain.Update.Gen (genBlockVersionData)
import           Test.Pos.Core.Gen (genCoin, genCoinPortion, genHashRaw,
                     genSharedSeed, genTextHash, genVssMaxTTL, genVssMinTTL)
import           Test.Pos.Crypto.Gen (genProtocolMagic, genRedeemPublicKey)

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

genGenesisProtocolConstants :: Gen GenesisProtocolConstants
genGenesisProtocolConstants =
    GenesisProtocolConstants
        <$> Gen.int (Range.constant 0 100)
        <*> genProtocolMagic
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
                      <*> genGenesisProtocolConstants
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
genGenesisAvvmBalances = GenesisAvvmBalances <$> customHashMapGen genRedeemPublicKey genCoin

----------------------------------------------------------------------------
-- Helper Generators
----------------------------------------------------------------------------

customHashMapGen
    :: (Hashable k, Eq k)
    => Gen k -> Gen v -> Gen (HM.HashMap k v)
customHashMapGen keyGen valGen =
    HM.fromList
        <$> (Gen.list (Range.linear 1 10) $ (,) <$> keyGen <*> valGen)
