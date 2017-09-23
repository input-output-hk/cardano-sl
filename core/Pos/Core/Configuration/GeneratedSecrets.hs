{-# LANGUAGE Rank2Types #-}

module Pos.Core.Configuration.GeneratedSecrets
       ( HasGeneratedSecrets
       , withGeneratedSecrets
       , generatedSecrets
       , genesisSecretKeys
       , genesisHdwSecretKeys
       , genesisVssSecretKeys
       ) where

import           Universum

import           Data.Reflection           (Given (..), give)

import           Pos.Core.Genesis.Generate (GeneratedSecrets (..))
import           Pos.Crypto.SecretSharing  (VssKeyPair)
import           Pos.Crypto.Signing.Types  (EncryptedSecretKey, SecretKey)

-- | Generated genesis data is always present.
-- This may be confusing: even in mainnet when we read the data from canonical
-- JSON, there's still 'GeneratedGenesisData'. It's "generated" from that
-- complete 'GenesisData'.
type HasGeneratedSecrets = Given (Maybe GeneratedSecrets)

withGeneratedSecrets :: Maybe GeneratedSecrets -> (HasGeneratedSecrets => r) -> r
withGeneratedSecrets = give

generatedSecrets :: HasGeneratedSecrets => Maybe GeneratedSecrets
generatedSecrets = given

genesisSecretKeys :: HasGeneratedSecrets => Maybe [SecretKey]
genesisSecretKeys = map (view _1) . gsSecretKeys <$> generatedSecrets

genesisHdwSecretKeys :: HasGeneratedSecrets => Maybe [EncryptedSecretKey]
genesisHdwSecretKeys = map (view _2) . gsSecretKeys <$> generatedSecrets

genesisVssSecretKeys :: HasGeneratedSecrets => Maybe [VssKeyPair]
genesisVssSecretKeys = map (view _3) . gsSecretKeys <$> generatedSecrets
