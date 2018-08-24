{-# LANGUAGE NumDecimals #-}

module Test.Pos.Core.Dummy
       ( dummyProtocolConstants
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

import           Pos.Core (ProtocolConstants (..), VssMaxTTL (..),
                     VssMinTTL (..), unsafeCoinPortionFromDouble,
                     withProtocolConstants)
import           Pos.Core.Genesis (FakeAvvmOptions (..),
                     GeneratedGenesisData (..), GeneratedSecrets (..),
                     GenesisAvvmBalances (..), GenesisInitializer (..),
                     PoorSecret, RichSecrets (..), TestnetBalanceOptions (..),
                     generateGenesisData, gsSecretKeys, gsSecretKeysPoor,
                     gsSecretKeysRich)
import           Pos.Crypto (SecretKey)

import           Test.Pos.Crypto.Dummy (dummyProtocolMagic)

dummyProtocolConstants :: ProtocolConstants
dummyProtocolConstants = ProtocolConstants
 { pcK = 10
 , pcVssMinTTL = VssMinTTL 2
 , pcVssMaxTTL = VssMaxTTL 6
 }

dummyGeneratedGenesisData :: GeneratedGenesisData
dummyGeneratedGenesisData =
    withProtocolConstants dummyProtocolConstants $ generateGenesisData
        dummyProtocolMagic
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
