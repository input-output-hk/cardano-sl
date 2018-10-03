module Test.Pos.Chain.Genesis.Example
       ( exampleGenesisAvvmBalances0
       , exampleGenesisAvvmBalances1
       , exampleGenesisAvvmBalances2
       , exampleStaticConfig_GCSpec0
       , exampleStaticConfig_GCSpec1
       , exampleStaticConfig_GCSpec2
       , exampleStaticConfig_GCSrc
       , exampleGenesisData0
       , exampleGenesisData1
       , exampleGenesisData2
       , exampleGenesisWStakeholders
       , exampleGenesisNonAvvmBalances0
       , exampleGenesisNonAvvmBalances1
       , exampleGenesisNonAvvmBalances2
       , exampleGenesisDelegation
       , exampleGenesisInitializer0
       , exampleGenesisInitializer1
       , exampleGenesisInitializer2
       , exampleGenesisProtocolConstants0
       , exampleGenesisProtocolConstants1
       , exampleGenesisProtocolConstants2
       ) where

import           Universum

import           Crypto.Error (throwCryptoError)
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import           Data.Maybe (fromJust)
import qualified Serokell.Util.Base16 as B16

import qualified Cardano.Crypto.Wallet as CC
import           Pos.Binary.Class (Raw (..))
import           Pos.Chain.Delegation (HeavyDlgIndex (..))
import           Pos.Chain.Genesis (FakeAvvmOptions (..),
                     GenesisAvvmBalances (..), GenesisData (..),
                     GenesisDelegation (..), GenesisInitializer (..),
                     GenesisNonAvvmBalances (..),
                     GenesisProtocolConstants (..), GenesisSpec (..),
                     GenesisWStakeholders (..), StaticConfig (..),
                     TestnetBalanceOptions (..))
import           Pos.Core (Coin (..), CoinPortion (..), EpochIndex (..),
                     Timestamp (..), VssMaxTTL (..), VssMinTTL (..),
                     addressHash)
import           Pos.Crypto (ProtocolMagic (..), ProtocolMagicId (..),
                     ProxyCert (..), ProxySecretKey (..), RedeemPublicKey,
                     RequiresNetworkMagic (..), abstractHash,
                     redeemDeterministicKeyGen)
import           Pos.Crypto.Signing (PublicKey (..), RedeemPublicKey (..))

import           Test.Pos.Chain.Ssc.Example (exampleVssCertificatesMap)
import           Test.Pos.Chain.Update.Example (exampleBlockVersionData0,
                     exampleBlockVersionData1, exampleBlockVersionData2)
import           Test.Pos.Core.ExampleHelpers (exampleAddress, exampleAddress1,
                     exampleAddress2, exampleAddress3, exampleAddress4,
                     exampleSharedSeed0, exampleSharedSeed1,
                     exampleSharedSeed2, exampleStakeholderId)
import           Test.Pos.Crypto.Bi (getBytes)


exampleStaticConfig_GCSrc :: StaticConfig
exampleStaticConfig_GCSrc =
    GCSrc "dRaMwdYsH3QA3dChe" (abstractHash (Raw "Test"))

exampleStaticConfig_GCSpec0 :: StaticConfig
exampleStaticConfig_GCSpec0 =
    GCSpec $ UnsafeGenesisSpec
        exampleGenesisAvvmBalances0
        exampleSharedSeed0
        exampleGenesisDelegation
        exampleBlockVersionData0
        exampleGenesisProtocolConstants0
        exampleGenesisInitializer0

exampleStaticConfig_GCSpec1 :: StaticConfig
exampleStaticConfig_GCSpec1 =
    GCSpec $ UnsafeGenesisSpec
        exampleGenesisAvvmBalances1
        exampleSharedSeed1
        exampleGenesisDelegation
        exampleBlockVersionData1
        exampleGenesisProtocolConstants1
        exampleGenesisInitializer1

exampleStaticConfig_GCSpec2 :: StaticConfig
exampleStaticConfig_GCSpec2 =
    GCSpec $ UnsafeGenesisSpec
        exampleGenesisAvvmBalances2
        exampleSharedSeed2
        exampleGenesisDelegation
        exampleBlockVersionData2
        exampleGenesisProtocolConstants2
        exampleGenesisInitializer2

exampleGenesisData0 :: GenesisData
exampleGenesisData0 =
    GenesisData
        { gdBootStakeholders = exampleGenesisWStakeholders
        , gdHeavyDelegation = exampleGenesisDelegation
        -- Timestamps are stored as seconds, so they are rounded to
        -- the nearest 10^6.
        , gdStartTime = Timestamp { getTimestamp = 421337000000 }
        , gdVssCerts = exampleVssCertificatesMap 10 4
        , gdNonAvvmBalances = exampleGenesisNonAvvmBalances0
        , gdBlockVersionData = exampleBlockVersionData0
        , gdProtocolConsts = exampleGenesisProtocolConstants0
        , gdAvvmDistr = exampleGenesisAvvmBalances0
        , gdFtsSeed = exampleSharedSeed0
        }

exampleGenesisData1 :: GenesisData
exampleGenesisData1 =
    GenesisData
        { gdBootStakeholders = exampleGenesisWStakeholders
        , gdHeavyDelegation = exampleGenesisDelegation
        , gdStartTime = Timestamp { getTimestamp = 3131000000 }
        , gdVssCerts = exampleVssCertificatesMap 0 10
        , gdNonAvvmBalances = exampleGenesisNonAvvmBalances1
        , gdBlockVersionData = exampleBlockVersionData1
        , gdProtocolConsts = exampleGenesisProtocolConstants1
        , gdAvvmDistr = exampleGenesisAvvmBalances1
        , gdFtsSeed = exampleSharedSeed1
        }

exampleGenesisData2 :: GenesisData
exampleGenesisData2 =
    GenesisData
        { gdBootStakeholders = exampleGenesisWStakeholders
        , gdHeavyDelegation = exampleGenesisDelegation
        , gdStartTime = Timestamp { getTimestamp = 3735000000 }
        , gdVssCerts = exampleVssCertificatesMap 8 5
        , gdNonAvvmBalances = exampleGenesisNonAvvmBalances2
        , gdBlockVersionData = exampleBlockVersionData2
        , gdProtocolConsts = exampleGenesisProtocolConstants2
        , gdAvvmDistr = exampleGenesisAvvmBalances2
        , gdFtsSeed = exampleSharedSeed2
        }

exampleGenesisWStakeholders :: GenesisWStakeholders
exampleGenesisWStakeholders =
    let mapSize = 1
        stakeholderIds = replicate mapSize exampleStakeholderId
        word16s :: [Word16]
        word16s = [1337]
    in  GenesisWStakeholders { getGenesisWStakeholders =
            M.fromList $ zip stakeholderIds word16s }

exampleGenesisNonAvvmBalances0 :: GenesisNonAvvmBalances
exampleGenesisNonAvvmBalances0 =
    GenesisNonAvvmBalances {getGenesisNonAvvmBalances =
        (HM.fromList [ (exampleAddress, coin)
                     , (exampleAddress1, coin1)
                     ]) }
  where
    coin  = Coin {getCoin = 36524597913081152}
    coin1 = Coin {getCoin = 37343863242999412}

exampleGenesisNonAvvmBalances1 :: GenesisNonAvvmBalances
exampleGenesisNonAvvmBalances1 =
    GenesisNonAvvmBalances {getGenesisNonAvvmBalances =
        (HM.fromList [ (exampleAddress, coin)
                     , (exampleAddress1, coin1)
                     , (exampleAddress2, coin2)
                     ]) }
  where
    coin  = Coin {getCoin = 1234567}
    coin1 = Coin {getCoin = 1337}
    coin2 = Coin {getCoin = 12552661871782587}

exampleGenesisNonAvvmBalances2 :: GenesisNonAvvmBalances
exampleGenesisNonAvvmBalances2 =
    GenesisNonAvvmBalances {getGenesisNonAvvmBalances =
        (HM.fromList [ (exampleAddress, coin)
                     , (exampleAddress1, coin1)
                     , (exampleAddress2, coin2)
                     , (exampleAddress3, coin3)
                     , (exampleAddress4, coin4)
                     ]) }
  where
    coin  = Coin {getCoin = 4873920820}
    coin1 = Coin {getCoin = 8274687}
    coin2 = Coin {getCoin = 12552661871782587}
    coin3 = Coin {getCoin = 189377823442}
    coin4 = Coin {getCoin = 4672189289312323}

exampleGenesisAvvmBalances0 :: GenesisAvvmBalances
exampleGenesisAvvmBalances0 =
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

exampleGenesisAvvmBalances1 :: GenesisAvvmBalances
exampleGenesisAvvmBalances1 =
    GenesisAvvmBalances {getGenesisAvvmBalances =
        (HM.fromList [(RedeemPublicKey fstRedKey
                     , Coin {getCoin = 434210906})
                     ,(RedeemPublicKey sndRedKey
                     ,Coin {getCoin = 172323403})
                     ]) }
  where
    fstRedKey :: Ed25519.PublicKey
    fstRedKey = throwCryptoError $
        Ed25519.publicKey $ hexToBS "a75fe437a908c252a8f3e9601df15d593a1a2a589c\
                                    \65b9519b0fab24f9396bdf"
    --
    sndRedKey :: Ed25519.PublicKey
    sndRedKey = throwCryptoError $
        Ed25519.publicKey $ hexToBS "296ca6970e61bf9854aea38d2cae9b3019a5c63fd28\
                                    \ff4a0e9d366c825c788e4"

exampleGenesisAvvmBalances2 :: GenesisAvvmBalances
exampleGenesisAvvmBalances2 =
    GenesisAvvmBalances {getGenesisAvvmBalances =
        (HM.fromList [(RedeemPublicKey fstRedKey
                     , Coin {getCoin = 630099400})
                     ,(RedeemPublicKey sndRedKey
                     ,Coin {getCoin = 37343863242999412})
                     ]) }
  where
    fstRedKey :: Ed25519.PublicKey
    fstRedKey = throwCryptoError $
        Ed25519.publicKey $ hexToBS "81a329ab75009b925efc26c334919509650b865dd6\
                                    \3d6f8db257e0245f04d324"
    --
    sndRedKey :: Ed25519.PublicKey
    sndRedKey = throwCryptoError $
        Ed25519.publicKey $ hexToBS "72043ce554335af1193adeb04639c1e1e2f7aaf203\
                                    \d997f8032856677c2ce05f"

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

exampleGenesisProtocolConstants0 :: GenesisProtocolConstants
exampleGenesisProtocolConstants0 = GenesisProtocolConstants
    { gpcK = 37
    , gpcProtocolMagic = ProtocolMagic (ProtocolMagicId 1783847074)
                                       NMMustBeJust
    , gpcVssMaxTTL = VssMaxTTL {getVssMaxTTL = 1477558317}
    , gpcVssMinTTL = VssMinTTL {getVssMinTTL = 744040476}}

exampleGenesisProtocolConstants1 :: GenesisProtocolConstants
exampleGenesisProtocolConstants1 = GenesisProtocolConstants
    { gpcK = 64
    , gpcProtocolMagic = ProtocolMagic
        { getProtocolMagicId = ProtocolMagicId 135977977
        , getRequiresNetworkMagic = NMMustBeJust
        }
    , gpcVssMaxTTL = VssMaxTTL {getVssMaxTTL = 126106167}
    , gpcVssMinTTL = VssMinTTL {getVssMinTTL = 310228653}}

exampleGenesisProtocolConstants2 :: GenesisProtocolConstants
exampleGenesisProtocolConstants2 = GenesisProtocolConstants
    { gpcK = 2
    , gpcProtocolMagic = ProtocolMagic
        { getProtocolMagicId = ProtocolMagicId 1780893186
        , getRequiresNetworkMagic = NMMustBeJust
        }
    , gpcVssMaxTTL = VssMaxTTL {getVssMaxTTL = 402296078}
    , gpcVssMinTTL = VssMinTTL {getVssMinTTL = 1341799941}}

exampleGenesisInitializer0 :: GenesisInitializer
exampleGenesisInitializer0 = GenesisInitializer
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

exampleGenesisInitializer1 :: GenesisInitializer
exampleGenesisInitializer1 = GenesisInitializer
    { giTestBalance = TestnetBalanceOptions
        { tboPoors = 6042739228893020332
        , tboRichmen = 6300032953394836094
        , tboTotalBalance = 15800731757646603171
        , tboRichmenShare = 3.7001504124217166
        , tboUseHDAddresses = False
        }
    , giFakeAvvmBalance = FakeAvvmOptions
        { faoCount = 1863091022267159357
        , faoOneBalance = 15821780831116413482
        }
    , giAvvmBalanceFactor = CoinPortion
        { getCoinPortion = 794919302027403
        }
    , giUseHeavyDlg = False
    , giSeed = 1
    }

exampleGenesisInitializer2 :: GenesisInitializer
exampleGenesisInitializer2 = GenesisInitializer
    { giTestBalance = TestnetBalanceOptions
        { tboPoors = 354896133480201129
        , tboRichmen = 6001143654143911879
        , tboTotalBalance = 911040566224079710
        , tboRichmenShare = 6.2349745920068775
        , tboUseHDAddresses = False
        }
    , giFakeAvvmBalance = FakeAvvmOptions
        { faoCount = 108112649620073880
        , faoOneBalance = 7896368278086760723
        }
    , giAvvmBalanceFactor = CoinPortion
        { getCoinPortion = 2106983080171
        }
    , giUseHeavyDlg = True
    , giSeed = 3
    }

hexToBS :: Text -> ByteString
hexToBS ts = case B16.decode ts of
    Left err -> error $ "decode failed: " <> show err
    Right bs -> bs
