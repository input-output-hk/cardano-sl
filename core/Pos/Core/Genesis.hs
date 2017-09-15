module Pos.Core.Genesis
       (
       -- * Data from 'GenesisSpec'
         genesisAvvmBalances
       , genesisInitializer
       , genesisDelegation

       , generatedGenesisData
       , genesisSecretKeys
       , genesisHdwSecretKeys
       , genesisVssSecretKeys
       , genesisCertificates

       -- * Utils
       , generateGenesisKeyPair
       , generateHdwGenesisSecretKey

       -- * Re-exports
       , module Pos.Core.Genesis.Constants
       , module Pos.Core.Genesis.Parser
       , module Pos.Core.Genesis.Testnet
       , module Pos.Core.Genesis.Types
       ) where

import           Universum

import qualified Data.Text                  as T
import           Formatting                 (int, sformat, (%))

import           Pos.Binary.Crypto          ()
import           Pos.Core.Vss               (VssCertificatesMap)
import           Pos.Crypto.SafeSigning     (EncryptedSecretKey, emptyPassphrase,
                                             safeDeterministicKeyGen)
import           Pos.Crypto.SecretSharing   (VssKeyPair)
import           Pos.Crypto.Signing         (PublicKey, SecretKey, deterministicKeyGen)

-- reexports
import           Pos.Core.Genesis.Canonical ()
import           Pos.Core.Genesis.Constants
import           Pos.Core.Genesis.Parser
import           Pos.Core.Genesis.Testnet
import           Pos.Core.Genesis.Types

----------------------------------------------------------------------------
-- Data taken from 'GenesisSpec'
----------------------------------------------------------------------------

-- | Genesis avvm balances.
genesisAvvmBalances :: GenesisAvvmBalances
genesisAvvmBalances = gsAvvmDistr genesisSpec

-- | Genesis initializer determines way of initialization
-- utxo, bootstrap stakeholders, etc.
genesisInitializer :: GenesisInitializer
genesisInitializer = gsInitializer genesisSpec

-- | 'GenesisDelegation' for production mode.
genesisDelegation :: GenesisDelegation
genesisDelegation = gsHeavyDelegation genesisSpec

generatedGenesisData :: GeneratedGenesisData
generatedGenesisData = generateTestnetOrMainnetData genesisInitializer

genesisCertificates :: VssCertificatesMap
genesisCertificates = ggdGtData generatedGenesisData

genesisSecretKeys :: Maybe [SecretKey]
genesisSecretKeys = map (view _1) <$> ggdSecretKeys generatedGenesisData

genesisHdwSecretKeys :: Maybe [EncryptedSecretKey]
genesisHdwSecretKeys = map (view _2) <$> ggdSecretKeys generatedGenesisData

genesisVssSecretKeys :: Maybe [VssKeyPair]
genesisVssSecretKeys = map (view _3) <$> ggdSecretKeys generatedGenesisData

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

generateGenesisKeyPair :: Int -> (PublicKey, SecretKey)
generateGenesisKeyPair =
    deterministicKeyGen .
    encodeUtf8 .
    T.take 32 . sformat ("My awesome 32-byte seed #" %int % "             ")

generateHdwGenesisSecretKey :: Int -> EncryptedSecretKey
generateHdwGenesisSecretKey =
    snd .
    flip safeDeterministicKeyGen emptyPassphrase .
    encodeUtf8 .
    T.take 32 . sformat ("My 32-byte hdw seed #" %int % "                  ")
