module Test.Pos.Launcher.Gen where

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Universum

import           Ntp.Client (NtpConfiguration (NtpConfiguration))
import           Pos.Chain.Block (BlockConfiguration (BlockConfiguration))
import           Pos.Chain.Delegation (DlgConfiguration (DlgConfiguration))
import           Pos.Chain.Genesis (StaticConfig (GCSrc))
import           Pos.Chain.Ssc (SscConfiguration (SscConfiguration))
import           Pos.Chain.Txp (TxpConfiguration (TxpConfiguration))
import           Pos.Chain.Update (UpdateConfiguration (UpdateConfiguration))
import           Pos.Configuration (NodeConfiguration (NodeConfiguration))
import           Pos.Crypto.Configuration
                     (RequiresNetworkMagic (RequiresMagic, RequiresNoMagic))
import           Pos.Crypto.Hashing (unsafeMkAbstractHash)
import           Pos.Launcher.Configuration (Configuration (Configuration),
                     ThrottleSettings (ThrottleSettings),
                     WalletConfiguration (WalletConfiguration))
import           Test.Pos.Chain.Txp.Gen (genTxValidationRulesConfig)
import           Test.Pos.Chain.Update.Gen (genApplicationName, genBlockVersion,
                     genSystemTag)

-- TODO, move a lot of the chain specific generators into cardano-sl-chain

genConfiguration :: Gen Configuration
genConfiguration = Configuration <$> genGenesis <*> genNtp <*> genUpdate <*> genSsc <*> genDlg <*> genTxp <*> genBlock <*> genNode <*> genWallet <*> genReqNetMagic <*> genTxValidationRulesConfig

genGenesis :: Gen StaticConfig
-- TODO, GCSpec not covered
genGenesis = GCSrc <$> genstring <*> (pure $ unsafeMkAbstractHash mempty)

genNtp :: Gen NtpConfiguration
genNtp = NtpConfiguration <$> Gen.list (Range.constant 1 4) genstring <*> Gen.integral (Range.constant 1 1000) <*> Gen.integral (Range.constant 1 1000)

genstring :: Gen String
genstring = Gen.string (Range.constant 1 10) Gen.alphaNum

genUpdate :: Gen UpdateConfiguration
genUpdate = UpdateConfiguration <$> genApplicationName <*> genBlockVersion <*> (pure 1) <*> genSystemTag

genSsc :: Gen SscConfiguration
genSsc = SscConfiguration <$> (pure 10) <*> (pure 3) <*> (pure False)

genDlg :: Gen DlgConfiguration
genDlg = DlgConfiguration <$> (pure 500) <*> (pure 30)

genTxp :: Gen TxpConfiguration
genTxp = TxpConfiguration <$> (pure 200) <*> (pure mempty)

genBlock :: Gen BlockConfiguration
genBlock = pure $ BlockConfiguration 1 2 3 4 5 6 7 8 9

genNode :: Gen NodeConfiguration
genNode = pure $ NodeConfiguration 1 2 3 4 True False True

genWallet :: Gen WalletConfiguration
genWallet = WalletConfiguration <$> Gen.maybe genThrottleSettings

genThrottleSettings :: Gen ThrottleSettings
genThrottleSettings = ThrottleSettings <$> genword64 <*> genword64 <*> genword64

genword64 :: Gen Word64
genword64 = Gen.word64 $ Range.constant 0 1000

genReqNetMagic :: Gen RequiresNetworkMagic
genReqNetMagic = Gen.choice [ pure RequiresMagic, pure RequiresNoMagic ]
