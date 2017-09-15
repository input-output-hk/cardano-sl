module Pos.Core.Genesis
       (
       -- * Data from 'GenesisSpec'
         genesisProdAvvmBalances
       , genesisProdInitializer
       , genesisProdDelegation

       , generatedGenesisData
       , genesisCertificates

       -- * Obsolete constants for dev mode
       , genesisDevKeyPairs
       , genesisDevPublicKeys
       , genesisDevSecretKeys
       , genesisDevHdwSecretKeys
       , genesisDevFlatDistr

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
import           System.IO.Unsafe           (unsafePerformIO)
import           System.Wlog                (usingLoggerName)

import           Pos.Binary.Crypto          ()
import           Pos.Core.Coin              (unsafeMulCoin)
import           Pos.Core.Constants         (genesisKeysN)
import           Pos.Core.Types             (mkCoin)
import           Pos.Core.Vss               (VssCertificatesMap)
import           Pos.Crypto.SafeSigning     (EncryptedSecretKey, emptyPassphrase,
                                             safeDeterministicKeyGen)
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
genesisProdAvvmBalances :: GenesisAvvmBalances
genesisProdAvvmBalances = gsAvvmDistr genesisSpec

-- | Genesis initializer determines way of initialization
-- utxo, bootstrap stakeholders, etc.
genesisProdInitializer :: GenesisInitializer
genesisProdInitializer = gsInitializer genesisSpec

-- | 'GenesisDelegation' for production mode.
genesisProdDelegation :: GenesisDelegation
genesisProdDelegation = gsHeavyDelegation genesisSpec

-- This unsafePerformIO is more or less safe,
-- because MonadIO needed for random.
-- Will be fixed soon.
generatedGenesisData :: GeneratedGenesisData
generatedGenesisData =
    unsafePerformIO $ usingLoggerName "core" $ generateTestnetOrMainnetData genesisProdInitializer
{-# NOINLINE generatedGenesisData #-}

genesisCertificates :: VssCertificatesMap
genesisCertificates = ggdGtData generatedGenesisData

----------------------------------------------------------------------------
-- Obsolete constants for dev mode
----------------------------------------------------------------------------

-- | List of pairs from 'SecretKey' with corresponding 'PublicKey'.
genesisDevKeyPairs :: [(PublicKey, SecretKey)]
genesisDevKeyPairs = map generateGenesisKeyPair [0 .. genesisKeysN - 1]

-- | List of 'PublicKey's in genesis.
genesisDevPublicKeys :: [PublicKey]
genesisDevPublicKeys = map fst genesisDevKeyPairs

-- | List of 'SecretKey's in genesis.
genesisDevSecretKeys :: [SecretKey]
genesisDevSecretKeys = map snd genesisDevKeyPairs

-- | List of 'SecretKey's in genesis for HD wallets.
genesisDevHdwSecretKeys :: [EncryptedSecretKey]
genesisDevHdwSecretKeys =
    map generateHdwGenesisSecretKey [0 .. genesisKeysN - 1]

-- | Default flat stakes distributed among 'genesisKeysN' (from constants).
genesisDevFlatDistr :: BalanceDistribution
genesisDevFlatDistr =
    FlatBalances genesisKeysN $
    mkCoin 10000 `unsafeMulCoin` (genesisKeysN :: Int)

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
