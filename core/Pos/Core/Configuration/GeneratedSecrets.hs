{-# LANGUAGE Rank2Types #-}

module Pos.Core.Configuration.GeneratedSecrets
       ( HasGeneratedSecrets
       , withGeneratedSecrets
       , generatedSecrets
       , genesisSecretsRich
       , genesisSecretsPoor
       , genesisSecretKeys
       , genesisSecretKeysRich
       , genesisSecretKeysPoor
       ) where

import           Universum

import           Data.Reflection (Given (..), give)

import           Pos.Core.Genesis.Generate (GeneratedSecrets (..), RichSecrets (..))
import           Pos.Crypto.Signing (EncryptedSecretKey, SecretKey, encToSecret)

-- | 'GeneratedSecrets' are known only when 'GenesisSpec' with
-- 'TestnetInitializer' is used to specify genesis. That's why we have
-- `Maybe` here.
type HasGeneratedSecrets = Given (Maybe GeneratedSecrets)

withGeneratedSecrets :: Maybe GeneratedSecrets -> (HasGeneratedSecrets => r) -> r
withGeneratedSecrets = give

generatedSecrets :: HasGeneratedSecrets => Maybe GeneratedSecrets
generatedSecrets = given

genesisSecretsRich :: HasGeneratedSecrets => Maybe [RichSecrets]
genesisSecretsRich = gsRichSecrets <$> generatedSecrets

genesisSecretsPoor :: HasGeneratedSecrets => Maybe [EncryptedSecretKey]
genesisSecretsPoor = gsPoorSecrets <$> generatedSecrets

genesisSecretKeys :: HasGeneratedSecrets => Maybe [SecretKey]
genesisSecretKeys = mappend <$> genesisSecretKeysRich <*> genesisSecretKeysPoor

genesisSecretKeysRich :: HasGeneratedSecrets => Maybe [SecretKey]
genesisSecretKeysRich = map rsPrimaryKey <$> genesisSecretsRich

genesisSecretKeysPoor :: HasGeneratedSecrets => Maybe [SecretKey]
genesisSecretKeysPoor = map encToSecret <$> genesisSecretsPoor
