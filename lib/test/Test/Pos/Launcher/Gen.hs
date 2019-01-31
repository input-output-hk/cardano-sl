module Test.Pos.Launcher.Gen where

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Universum

import           Ntp.Client (NtpConfiguration (NtpConfiguration))
import           Pos.Configuration (NodeConfiguration (NodeConfiguration))
import           Pos.Crypto (ProtocolMagic)
import           Pos.Launcher.Configuration (Configuration (Configuration),
                     ThrottleSettings (ThrottleSettings),
                     WalletConfiguration (WalletConfiguration))
import           Test.Pos.Chain.Block.Gen (genBlockConfiguration)
import           Test.Pos.Chain.Delegation.Gen (genDlgConfiguration)
import           Test.Pos.Chain.Genesis.Gen (genStaticConfig)
import           Test.Pos.Chain.Ssc.Gen (genSscConfiguration)
import           Test.Pos.Chain.Txp.Gen (genTxValidationRulesConfig,
                     genTxpConfiguration)
import           Test.Pos.Chain.Update.Gen (genUpdateConfiguration)
import           Test.Pos.Crypto.Gen (genRequiresNetworkMagic)


genConfiguration :: ProtocolMagic -> Gen Configuration
genConfiguration pm = Configuration <$> genStaticConfig pm <*> genNtpConfiguration <*> genUpdateConfiguration <*> genSscConfiguration <*> genDlgConfiguration <*> genTxpConfiguration <*> genBlockConfiguration <*> genNode <*> genWallet <*> genRequiresNetworkMagic <*> genTxValidationRulesConfig

genNtpConfiguration :: Gen NtpConfiguration
genNtpConfiguration = NtpConfiguration <$> Gen.list (Range.constant 1 4) genString <*> Gen.integral (Range.constant 1 1000) <*> Gen.integral (Range.constant 1 1000)

genString :: Gen String
genString = Gen.string (Range.constant 1 10) Gen.alphaNum

genNode :: Gen NodeConfiguration
genNode = NodeConfiguration <$> Gen.int Range.constantBounded
                            <*> Gen.int Range.constantBounded
                            <*> Gen.int Range.constantBounded
                            <*> Gen.int Range.constantBounded
                            <*> Gen.bool
                            <*> Gen.bool
                            <*> Gen.bool

genWallet :: Gen WalletConfiguration
genWallet = WalletConfiguration <$> Gen.maybe genThrottleSettings

genThrottleSettings :: Gen ThrottleSettings
genThrottleSettings = ThrottleSettings <$> genWord64 <*> genWord64 <*> genWord64

genWord64 :: Gen Word64
genWord64 = Gen.word64 $ Range.constant 0 1000
