{-# LANGUAGE Rank2Types #-}

module Pos.Core.Configuration.GeneratedGenesisData
       ( HasGeneratedGenesisData
       , withGeneratedGenesisData
       , generatedGenesisData
       , genesisSecretKeys
       , genesisHdwSecretKeys
       , genesisVssSecretKeys
       , genesisCertificates
       ) where

import           Universum

import           Data.Reflection            (Given (..), give)

import           Pos.Core.Genesis.Generate  (GeneratedGenesisData (..))
import           Pos.Core.Vss.Types         (VssCertificatesMap)
import           Pos.Crypto.SecretSharing   (VssKeyPair)
import           Pos.Crypto.Signing.Types   (SecretKey, EncryptedSecretKey)

-- | Generated genesis data is always present.
-- This may be confusing: even in mainnet when we read the data from canonical
-- JSON, there's still 'GeneratedGenesisData'. It's "generated" from that
-- complete 'GenesisData'.
type HasGeneratedGenesisData = Given GeneratedGenesisData

withGeneratedGenesisData :: GeneratedGenesisData -> (HasGeneratedGenesisData => r) -> r
withGeneratedGenesisData = give

generatedGenesisData :: HasGeneratedGenesisData => GeneratedGenesisData
generatedGenesisData = given

genesisSecretKeys :: HasGeneratedGenesisData => Maybe [SecretKey]
genesisSecretKeys = map (view _1) <$> ggdSecretKeys generatedGenesisData

genesisHdwSecretKeys :: HasGeneratedGenesisData => Maybe [EncryptedSecretKey]
genesisHdwSecretKeys = map (view _2) <$> ggdSecretKeys generatedGenesisData

genesisVssSecretKeys :: HasGeneratedGenesisData => Maybe [VssKeyPair]
genesisVssSecretKeys = map (view _3) <$> ggdSecretKeys generatedGenesisData

genesisCertificates :: HasGeneratedGenesisData => VssCertificatesMap
genesisCertificates = ggdGtData generatedGenesisData
