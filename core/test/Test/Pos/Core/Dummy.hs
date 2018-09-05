{-# LANGUAGE NumDecimals #-}

module Test.Pos.Core.Dummy
       ( dummyConfig
       , dummyConfigStartTime
       , dummyProtocolConstants
       , dummyK
       , dummyEpochSlots
       , dummySlotSecurityParam
       , dummyGenesisInitializer
       , dummyGenesisAvvmBalances
       , dummyGeneratedGenesisData
       , dummyGeneratedSecrets
       , dummyGenesisSecretKeys
       , dummyGenesisSecretKeysRich
       , dummyGenesisSecretKeysPoor
       , dummyGenesisSecretsRich
       , dummyGenesisSecretsPoor
       , dummyCoreConfiguration
       , dummyGenesisSpec
       , dummyGenesisData
       , dummyGenesisDataStartTime
       , dummyGenesisHash
       ) where

import           Universum

import           Data.Coerce (coerce)

import           Pos.Core (BlockCount, Coeff (..), Config (..),
                     CoreConfiguration (..), EpochIndex (..),
                     GenesisConfiguration (..), GenesisHash (..),
                     ProtocolConstants (..), SharedSeed (..), SlotCount,
                     Timestamp, TxFeePolicy (..), TxSizeLinear (..),
                     VssMaxTTL (..), VssMinTTL (..), kEpochSlots,
                     kSlotSecurityParam, pcBlkSecurityParam,
                     unsafeCoinPortionFromDouble)
import           Pos.Core.Genesis (FakeAvvmOptions (..),
                     GeneratedGenesisData (..), GeneratedSecrets (..),
                     GenesisAvvmBalances (..), GenesisData (..),
                     GenesisInitializer (..), GenesisSpec (..), PoorSecret,
                     RichSecrets (..), TestnetBalanceOptions (..),
                     generateGenesisData,
                     genesisProtocolConstantsFromProtocolConstants,
                     gsSecretKeys, gsSecretKeysPoor, gsSecretKeysRich,
                     noGenesisDelegation)
import           Pos.Core.Update (BlockVersionData (..), SoftforkRule (..))
import           Pos.Crypto (SecretKey, unsafeHash)

import           Test.Pos.Crypto.Dummy (dummyProtocolMagic)

dummyConfig :: Config
dummyConfig = dummyConfigStartTime 0

dummyConfigStartTime :: Timestamp -> Config
dummyConfigStartTime systemStart = Config
    { configProtocolMagic     = dummyProtocolMagic
    , configProtocolConstants = dummyProtocolConstants
    , configGeneratedSecrets  = Just dummyGeneratedSecrets
    , configGenesisData       = dummyGenesisDataStartTime systemStart
    , configGenesisHash       = dummyGenesisHash
    }

dummyProtocolConstants :: ProtocolConstants
dummyProtocolConstants = ProtocolConstants
    { pcK         = 10
    , pcVssMinTTL = VssMinTTL 2
    , pcVssMaxTTL = VssMaxTTL 6
    }

dummyK :: BlockCount
dummyK = pcBlkSecurityParam dummyProtocolConstants

dummyEpochSlots :: SlotCount
dummyEpochSlots = kEpochSlots dummyK

dummySlotSecurityParam :: SlotCount
dummySlotSecurityParam = kSlotSecurityParam dummyK

dummyGeneratedGenesisData :: GeneratedGenesisData
dummyGeneratedGenesisData = generateGenesisData dummyProtocolMagic
                                                dummyProtocolConstants
                                                dummyGenesisInitializer
                                                dummyGenesisAvvmBalances

dummyGeneratedSecrets :: GeneratedSecrets
dummyGeneratedSecrets = ggdSecrets dummyGeneratedGenesisData

dummyGenesisSecretsRich :: [RichSecrets]
dummyGenesisSecretsRich = gsRichSecrets dummyGeneratedSecrets

dummyGenesisSecretsPoor :: [PoorSecret]
dummyGenesisSecretsPoor = gsPoorSecrets dummyGeneratedSecrets

dummyGenesisSecretKeys :: [SecretKey]
dummyGenesisSecretKeys = gsSecretKeys dummyGeneratedSecrets

dummyGenesisSecretKeysRich :: [SecretKey]
dummyGenesisSecretKeysRich = gsSecretKeysRich dummyGeneratedSecrets

dummyGenesisSecretKeysPoor :: [SecretKey]
dummyGenesisSecretKeysPoor = gsSecretKeysPoor dummyGeneratedSecrets

dummyCoreConfiguration :: CoreConfiguration
dummyCoreConfiguration = CoreConfiguration (GCSpec dummyGenesisSpec) 0

dummyGenesisSpec :: GenesisSpec
dummyGenesisSpec = UnsafeGenesisSpec
    dummyGenesisAvvmBalances
    dummyFtsSeed
    noGenesisDelegation
    dummyBlockVersionData
    (genesisProtocolConstantsFromProtocolConstants dummyProtocolConstants
                                                   dummyProtocolMagic
    )
    dummyGenesisInitializer

dummyGenesisAvvmBalances :: GenesisAvvmBalances
dummyGenesisAvvmBalances = GenesisAvvmBalances mempty

dummyFtsSeed :: SharedSeed
dummyFtsSeed = SharedSeed "c2tvdm9yb2RhIEdndXJkYSBib3JvZGEgcHJvdm9kYSA="

dummyBlockVersionData :: BlockVersionData
dummyBlockVersionData = BlockVersionData
    0
    7000
    2000000
    2000000
    4096
    700
    (unsafeCoinPortionFromDouble 0.01)
    (unsafeCoinPortionFromDouble 0.005)
    (unsafeCoinPortionFromDouble 0.001)
    (unsafeCoinPortionFromDouble 0.1)
    10
    (SoftforkRule (unsafeCoinPortionFromDouble 0.9)
                  (unsafeCoinPortionFromDouble 0.6)
                  (unsafeCoinPortionFromDouble 0.05)
    )
    (TxFeePolicyTxSizeLinear $ TxSizeLinear (Coeff 155381) (Coeff 43.946))
    (EpochIndex maxBound)

dummyGenesisInitializer :: GenesisInitializer
dummyGenesisInitializer = GenesisInitializer
    (TestnetBalanceOptions 12 4 6e17 0.99 True)
    (FakeAvvmOptions 10 100000)
    (unsafeCoinPortionFromDouble 1)
    True
    0

dummyGenesisData :: GenesisData
dummyGenesisData = dummyGenesisDataStartTime 0

dummyGenesisDataStartTime :: Timestamp -> GenesisData
dummyGenesisDataStartTime systemStart = GenesisData
    { gdBootStakeholders = ggdBootStakeholders dummyGeneratedGenesisData
    , gdHeavyDelegation  = ggdDelegation dummyGeneratedGenesisData
    , gdStartTime        = systemStart
    , gdVssCerts         = ggdVssCerts dummyGeneratedGenesisData
    , gdNonAvvmBalances  = ggdNonAvvm dummyGeneratedGenesisData
    , gdBlockVersionData = dummyBlockVersionData
    , gdProtocolConsts   = gsProtocolConstants dummyGenesisSpec
    , gdAvvmDistr        = ggdAvvm dummyGeneratedGenesisData
    , gdFtsSeed          = dummyFtsSeed
    }

dummyGenesisHash :: GenesisHash
dummyGenesisHash = GenesisHash $ coerce $ unsafeHash @Text "patak"
