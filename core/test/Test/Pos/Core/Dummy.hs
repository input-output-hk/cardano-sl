{-# LANGUAGE NumDecimals #-}

module Test.Pos.Core.Dummy
       ( dummyConfig
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
       ) where

import           Universum

import           Pos.Core (BlockCount, Config (..), ProtocolConstants (..),
                     SlotCount, VssMaxTTL (..), VssMinTTL (..), kEpochSlots,
                     kSlotSecurityParam, pcBlkSecurityParam,
                     unsafeCoinPortionFromDouble)
import           Pos.Core.Genesis (FakeAvvmOptions (..),
                     GeneratedGenesisData (..), GeneratedSecrets (..),
                     GenesisAvvmBalances (..), GenesisInitializer (..),
                     PoorSecret, RichSecrets (..), TestnetBalanceOptions (..),
                     generateGenesisData, gsSecretKeys, gsSecretKeysPoor,
                     gsSecretKeysRich)
import           Pos.Crypto (SecretKey)

import           Test.Pos.Crypto.Dummy (dummyProtocolMagic)

dummyConfig :: Config
dummyConfig = Config
    { configProtocolMagic = dummyProtocolMagic
    , configProtocolConstants = dummyProtocolConstants
    , configGeneratedSecrets = Just dummyGeneratedSecrets
    }

dummyProtocolConstants :: ProtocolConstants
dummyProtocolConstants = ProtocolConstants
 { pcK = 10
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

dummyGenesisInitializer :: GenesisInitializer
dummyGenesisInitializer = GenesisInitializer
    (TestnetBalanceOptions 12 4 6e17 0.99 True)
    (FakeAvvmOptions 10 100000)
    (unsafeCoinPortionFromDouble 1)
    True
    0

dummyGenesisAvvmBalances :: GenesisAvvmBalances
dummyGenesisAvvmBalances = GenesisAvvmBalances mempty
