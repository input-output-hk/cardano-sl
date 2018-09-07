module Test.Pos.Chain.Genesis.Example
       ( exampleGenesisAvvmBalances
       , exampleStaticConfig_GCSpec
       , exampleStaticConfig_GCSrc
       , exampleGenesisDelegation
       , exampleGenesisInitializer
       , exampleProtocolConstants
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import           Data.Maybe (fromJust)
import qualified Serokell.Util.Base16 as B16

import qualified Cardano.Crypto.Wallet as CC
import           Pos.Binary.Class (Raw (..))
import           Pos.Chain.Genesis (FakeAvvmOptions (..),
                     GenesisAvvmBalances (..), GenesisDelegation (..),
                     GenesisInitializer (..), GenesisProtocolConstants (..),
                     GenesisSpec (..), StaticConfig (..),
                     TestnetBalanceOptions (..))
import           Pos.Core (Coin (..), CoinPortion (..), EpochIndex (..),
                     VssMaxTTL (..), VssMinTTL (..), addressHash)
import           Pos.Core.Delegation (HeavyDlgIndex (..))
import           Pos.Crypto (ProtocolMagic (..), ProxyCert (..),
                     ProxySecretKey (..), RedeemPublicKey, abstractHash,
                     redeemDeterministicKeyGen)
import           Pos.Crypto.Signing (PublicKey (..))

import           Test.Pos.Core.ExampleHelpers (exampleBlockVersionData,
                     exampleSharedSeed)
import           Test.Pos.Crypto.Bi (getBytes)


exampleStaticConfig_GCSrc :: StaticConfig
exampleStaticConfig_GCSrc =
    GCSrc "dRaMwdYsH3QA3dChe" (abstractHash (Raw "Test"))

exampleStaticConfig_GCSpec :: StaticConfig
exampleStaticConfig_GCSpec =
    GCSpec $ UnsafeGenesisSpec
        exampleGenesisAvvmBalances
        exampleSharedSeed
        exampleGenesisDelegation
        exampleBlockVersionData
        exampleProtocolConstants
        exampleGenesisInitializer

exampleGenesisAvvmBalances :: GenesisAvvmBalances
exampleGenesisAvvmBalances =
    GenesisAvvmBalances
        { getGenesisAvvmBalances = HM.fromList
            [ ( exampleRedeemPublicKey' (0, 32)
              , Coin {getCoin = 36524597913081152}
              )
            , ( exampleRedeemPublicKey' (32, 32)
              , Coin {getCoin = 37343863242999412}
              )
            ]
        }
    where
        exampleRedeemPublicKey' :: (Int, Int) -> RedeemPublicKey
        exampleRedeemPublicKey' (m, n) = fromJust (fst <$> redeemDeterministicKeyGen (getBytes m n))

exampleGenesisDelegation :: GenesisDelegation
exampleGenesisDelegation = UnsafeGenesisDelegation (HM.fromList
    [( addressHash issuePubKey
     , UnsafeProxySecretKey
         { pskOmega =
             HeavyDlgIndex $ EpochIndex 68300481033
         , pskIssuerPk = issuePubKey
         , pskDelegatePk =
             PublicKey (CC.XPub { CC.xpubPublicKey = pskDelPubKey
                                , CC.xpubChaincode = pskDelChainCode})
         , pskCert =
             ProxyCert (fromRight (error "Something went wrong") $ sig)
         }
      )]
    )
  where
    issuePubKey = PublicKey (CC.XPub { CC.xpubPublicKey = pskPubKey
                                     , CC.xpubChaincode = pskChainCode})
    sig = CC.xsignature (hexToBS "bae5422af5405e3803154a4ad986da5d14cf624d670\
                                 \1c5c78a79ec73777f74e13973af83752114d9f18166\
                                 \085997fc81e432cab7fee99a275d8bf138ad04e103")
    pskPubKey = hexToBS "e2a1773a2a82d10c30890cbf84eccbdc1aaaee920496424d36e8\
                        \68039d9cb519"
    pskChainCode = CC.ChainCode (hexToBS "21b25efe033d9b00d4f02ccd9cdabcec332\
                                         \abbc6fdf883ca5bf3a8aff4aac27e")
    pskDelPubKey = hexToBS "ddca69bfeac14c013304da88ac032ee63281ab036c1b1b918\
                           \8e4b174b303f43e"
    pskDelChainCode = CC.ChainCode (hexToBS "55163b178e999b9fd50637b2edab8c85\
                                            \8a879ac3c4bd3e610095419a19696573")

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

hexToBS :: Text -> ByteString
hexToBS ts = case B16.decode ts of
    Left err -> error $ "decode failed: " <> show err
    Right bs -> bs
