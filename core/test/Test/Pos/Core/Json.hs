module Test.Pos.Core.Json where

import qualified Cardano.Crypto.Wallet as CC
import           Crypto.Error (CryptoFailable (..))
import qualified Crypto.PubKey.Ed25519 as Ed25519
import           Data.Fixed
import qualified Data.HashMap.Strict as HM
import           Data.Time.Units (Millisecond)
import           Hedgehog (Property)
import qualified Hedgehog as H
import           Pos.Binary.Class (Raw (..))
import           Pos.Core.Common (Coeff (..), Coin (..), CoinPortion (..),
                     ScriptVersion, SharedSeed (..), TxFeePolicy (..),
                     TxSizeLinear (..))
import           Pos.Core.Configuration
import           Pos.Core.Delegation (HeavyDlgIndex (..))
import           Pos.Core.Genesis (FakeAvvmOptions (..),
                     GenesisAvvmBalances (..), GenesisDelegation (..),
                     GenesisInitializer (..), GenesisProtocolConstants (..),
                     GenesisSpec (..), TestnetBalanceOptions (..))
import           Pos.Core.JsonLog.LogEvents (InvReqDataFlowLog (..))
import           Pos.Core.ProtocolConstants (VssMaxTTL (..), VssMinTTL (..))
import           Pos.Core.Slotting (EpochIndex (..), FlatSlotId)
import           Pos.Core.Update (BlockVersionData (..), SoftforkRule (..))
import           Pos.Crypto (ProtocolMagic (..))
import           Pos.Crypto.Hashing (abstractHash)
import           Pos.Crypto.Signing (ProxyCert (..), ProxySecretKey (..),
                     PublicKey (..), RedeemPublicKey (..))
import           Serokell.Data.Memory.Units (Byte)
import           Test.Pos.Core.Bi (feedPM)
import           Test.Pos.Core.Gen (genBlockVersionData, genByte, genCoin,
                     genCoinPortion, genEpochIndex, genFlatSlotId,
                     genGenesisAvvmBalances, genGenesisConfiguration,
                     genGenesisDelegation, genGenesisInitializer,
                     genGenesisProtocolConstants, genInvReqDataFlowLog,
                     genSharedSeed, genSoftforkRule, genTxFeePolicy)
import           Test.Pos.Crypto.Bi (getBytes)
import           Test.Pos.Crypto.Gen (genRedeemPublicKey)
import           Test.Pos.Util.Gen (genMillisecond)
import           Test.Pos.Util.Golden (discoverGolden, eachOf, goldenTestJSON)
import           Test.Pos.Util.Tripping (discoverRoundTrip,
                     roundTripsAesonBuildable, roundTripsAesonShow)
import           Universum

--------------------------------------------------------------------------------
-- GensisConfiguration
--------------------------------------------------------------------------------

golden_GenesisConfiguration_GCSpec :: Property
golden_GenesisConfiguration_GCSpec =
    goldenTestJSON
        exampleGenesisConfiguration_GCSpec
            "test/golden/GenesisConfiguration_GCSpec"

golden_GenesisConfiguration_GCSrc :: Property
golden_GenesisConfiguration_GCSrc =
    goldenTestJSON
        exampleGenesisConfiguration_GCSrc
            "test/golden/GenesisConfiguration_GCSrc"

roundTripGenesisConfiguration :: Property
roundTripGenesisConfiguration =
    eachOf 100 (feedPM genGenesisConfiguration) roundTripsAesonShow

--------------------------------------------------------------------------------
-- GenesisAvvmBalances
--------------------------------------------------------------------------------

roundTripGenesisAvvmBalances :: Property
roundTripGenesisAvvmBalances =
     eachOf 100 genGenesisAvvmBalances roundTripsAesonShow

--------------------------------------------------------------------------------
-- RedeemPublicKey
--------------------------------------------------------------------------------

roundTripRedeemPublicKey :: Property
roundTripRedeemPublicKey = eachOf 1000 genRedeemPublicKey roundTripsAesonShow

--------------------------------------------------------------------------------
-- Coin
--------------------------------------------------------------------------------

roundTripCoin :: Property
roundTripCoin = eachOf 1000 genCoin roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- SharedSeed
--------------------------------------------------------------------------------

roundTripSharedSeed :: Property
roundTripSharedSeed = eachOf 1000 genSharedSeed roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- GenesisDelegation
--------------------------------------------------------------------------------

roundTripGenesisDelegation :: Property
roundTripGenesisDelegation =
    eachOf 100 (feedPM genGenesisDelegation) roundTripsAesonShow

--------------------------------------------------------------------------------
-- BlockVersionData
--------------------------------------------------------------------------------

roundTripBlockVersionData :: Property
roundTripBlockVersionData =
    eachOf 1000 genBlockVersionData roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- Millisecond
--------------------------------------------------------------------------------

roundTripMillisecond :: Property
roundTripMillisecond = eachOf 1000 genMillisecond roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- Byte
--------------------------------------------------------------------------------

roundTripByte :: Property
roundTripByte = eachOf 1000 genByte roundTripsAesonShow

--------------------------------------------------------------------------------
-- CoinPortion
--------------------------------------------------------------------------------

roundTripCoinPortion :: Property
roundTripCoinPortion = eachOf 1000 genCoinPortion roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- FlatSlotId
--------------------------------------------------------------------------------

roundTripFlatSlotId :: Property
roundTripFlatSlotId = eachOf 1000 genFlatSlotId roundTripsAesonShow

--------------------------------------------------------------------------------
-- SoftforkRule
--------------------------------------------------------------------------------

roundTripSoftforkRule :: Property
roundTripSoftforkRule = eachOf 1000 genSoftforkRule roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- TxFeePolicy
--------------------------------------------------------------------------------

roundTripTxFeePolicy :: Property
roundTripTxFeePolicy = eachOf 1000 genTxFeePolicy roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- EpochIndex
--------------------------------------------------------------------------------

roundTripEpochIndex :: Property
roundTripEpochIndex = eachOf 1000 genEpochIndex roundTripsAesonBuildable


--------------------------------------------------------------------------------
-- ProtocolConstants
--------------------------------------------------------------------------------

roundTripProtocolConstants :: Property
roundTripProtocolConstants =
    eachOf 1000 genGenesisProtocolConstants roundTripsAesonShow

--------------------------------------------------------------------------------
-- GenesisInitializer
--------------------------------------------------------------------------------

roundTripGenesisInitializer :: Property
roundTripGenesisInitializer =
    eachOf 1000 genGenesisInitializer roundTripsAesonShow

--------------------------------------------------------------------------------
-- InvReqDataFlowLog
--------------------------------------------------------------------------------

golden_InvReqDataFlowLog_InvReqAccepted :: Property
golden_InvReqDataFlowLog_InvReqAccepted =
    goldenTestJSON
        (InvReqAccepted 1 2 3 4)
            "test/golden/InvReqDataFlowLog_InvReqAccepted"

golden_InvReqDataFlowLog_InvReqRejected :: Property
golden_InvReqDataFlowLog_InvReqRejected =
    goldenTestJSON
        (InvReqRejected 1 2)
            "test/golden/InvReqDataFlowLog_InvReqRejected"

golden_InvReqDataFlowLog_InvReqException :: Property
golden_InvReqDataFlowLog_InvReqException =
    goldenTestJSON
        (InvReqException "test")
            "test/golden/InvReqDataFlowLog_InvReqException"

roundTripInvReqDataFlowLog :: Property
roundTripInvReqDataFlowLog =
    eachOf 1000 genInvReqDataFlowLog roundTripsAesonShow

--------------------------------------------------------------------------------
-- Example golden datatypes
--------------------------------------------------------------------------------

exampleGenesisConfiguration_GCSrc :: GenesisConfiguration
exampleGenesisConfiguration_GCSrc =
    GCSrc "dRaMwdYsH3QA3dChe" (abstractHash (Raw "Test"))

exampleGenesisConfiguration_GCSpec :: GenesisConfiguration
exampleGenesisConfiguration_GCSpec =
    GCSpec $ UnsafeGenesisSpec
        exampleGenesisAvvmBalances
        exampleSharedSeed
        exampleGenesisDelegation
        exampleBlockVersionData
        exampleProtocolConstants
        exampleGenesisInitializer

exampleGenesisAvvmBalances :: GenesisAvvmBalances
exampleGenesisAvvmBalances =
    GenesisAvvmBalances {getGenesisAvvmBalances =
        HM.fromList [(RedeemPublicKey (unsafePublicKey fstRedKey)
                     , Coin {getCoin = 36524597913081152})
                     ,(RedeemPublicKey (unsafePublicKey sndRedKey)
                     ,Coin {getCoin = 37343863242999412})
                     ] }
  where
    unsafePublicKey :: ByteString -> Ed25519.PublicKey
    unsafePublicKey bytes = case Ed25519.publicKey bytes of
        CryptoFailed e -> error (show e)
        CryptoPassed r -> r
    fstRedKey = "\254\156\235\217{]\130W\183LfJ\240"
    sndRedKey = "\254\156\235\217{]\130W\183LfJ\240\RS\224"

exampleSharedSeed :: SharedSeed
exampleSharedSeed = SharedSeed (getBytes 8 32)

exampleGenesisDelegation :: GenesisDelegation
exampleGenesisDelegation = UnsafeGenesisDelegation (HM.fromList
    [(abstractHash (PublicKey (CC.XPub {CC.xpubPublicKey = pubKey1
    , CC.xpubChaincode = CC.ChainCode "Test"}))
    , UnsafeProxySecretKey {pskOmega = HeavyDlgIndex $ EpochIndex 68300481033
    , pskIssuerPk = PublicKey (CC.XPub {CC.xpubPublicKey = pskPubKey
    , CC.xpubChaincode = CC.ChainCode "Test"})
    , pskDelegatePk = PublicKey (CC.XPub {CC.xpubPublicKey = pskDelPubKey
    , CC.xpubChaincode = CC.ChainCode "Test"})
    , pskCert = ProxyCert (fromRight (error "Something went wrong") $ sig)})])
  where
    sig = CC.xsignature "\186\229B*\245@^8\ETX\NAKJJ\217\134\218]\DC4\207\
                        \bMg\SOH\197\199\138y\236sw\DELt\225\&9s\175\131\
                        \u!\DC4\217\241\129f\bY\151\252\129\228\&\
                        \2\202\183\254\233\154']\139\241\&8\173\EOT\225\ETX"
    pubKey1 = "\145\&3\131kUF\226\131\253M\174\157;w>\156k"
    pskPubKey = "\DC1\RS\145\&3\131kUF\226\131\253M\174\157;w>\156k"
    pskDelPubKey = "\132\248\DC1\RS\145\&3\131kUF\226\131\253M\174\157;w>\156k"

exampleBlockVersionData :: BlockVersionData
exampleBlockVersionData = BlockVersionData
                              (999 :: ScriptVersion)
                              (999 :: Millisecond)
                              (999 :: Byte)
                              (999 :: Byte)
                              (999 :: Byte)
                              (999 :: Byte)
                              (CoinPortion 99)
                              (CoinPortion 99)
                              (CoinPortion 99)
                              (CoinPortion 99)
                              (99 :: FlatSlotId)
                              sfrule
                              (TxFeePolicyTxSizeLinear tslin)
                              (EpochIndex 99)
    where
        tslin = TxSizeLinear c1' c2'
        c1' = Coeff (MkFixed 999)
        c2' = Coeff (MkFixed 77)
        sfrule = SoftforkRule
            (CoinPortion 99) (CoinPortion 99) (CoinPortion 99)

exampleProtocolConstants :: GenesisProtocolConstants
exampleProtocolConstants = GenesisProtocolConstants
    { gpcK = 37
    , gpcProtocolMagic = ProtocolMagic {getProtocolMagic = 1783847074}
    , gpcVssMaxTTL = VssMaxTTL {getVssMaxTTL = 1477558317}
    , gpcVssMinTTL = VssMinTTL {getVssMinTTL = 744040476}}

exampleGenesisInitializer :: GenesisInitializer
exampleGenesisInitializer = GenesisInitializer
    {giTestBalance = TestnetBalanceOptions
        {tboPoors = 2448641325904532856
        , tboRichmen = 14071205313513960321
        , tboTotalBalance = 10953275486128625216
        , tboRichmenShare = 4.2098713311249885
        , tboUseHDAddresses = True}
        , giFakeAvvmBalance = FakeAvvmOptions
            {faoCount = 17853231730478779264
            , faoOneBalance = 15087947214890024355}
            , giAvvmBalanceFactor = CoinPortion
                 {getCoinPortion = 366832547637728}
                 , giUseHeavyDlg = False
                 , giSeed = 0}

tests :: IO Bool
tests = (&&) <$> H.checkSequential $$discoverGolden
             <*> H.checkParallel $$discoverRoundTrip
