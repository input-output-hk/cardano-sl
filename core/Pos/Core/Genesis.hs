module Pos.Core.Genesis
       (
       -- * Data from 'GenesisSpec'
         genesisProdAvvmBalances
       , genesisProdInitializer
       , genesisProdDelegation

       -- ** Genesis BlockVersionData
       , genesisBlockVersionData
       , genesisMpcThd
       , genesisHeavyDelThd
       , genesisUpdateVoteThd
       , genesisSlotDuration
       , genesisMaxBlockSize

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
       , module Pos.Core.Genesis.Types
       , module Pos.Core.Genesis.Parser
       ) where

import           Universum

import qualified Data.Text                  as T
import           Data.Time.Units            (Millisecond)
import           Formatting                 (int, sformat, (%))
import           Serokell.Data.Memory.Units (Byte)

import           Pos.Binary.Crypto          ()
import           Pos.Core.Coin              (unsafeMulCoin)
import           Pos.Core.Constants         (genesisKeysN)
import           Pos.Core.Types             (BlockVersionData (..), CoinPortion, mkCoin)
import           Pos.Crypto.SafeSigning     (EncryptedSecretKey, emptyPassphrase,
                                             safeDeterministicKeyGen)
import           Pos.Crypto.Signing         (PublicKey, SecretKey, deterministicKeyGen)

-- reexports
import           Pos.Core.Genesis.Canonical ()
import           Pos.Core.Genesis.Parser
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

-- | Genesis 'BlockVersionData'.
genesisBlockVersionData :: BlockVersionData
genesisBlockVersionData = gsBlockVersionData genesisSpec

genesisMpcThd :: CoinPortion
genesisMpcThd = bvdMpcThd genesisBlockVersionData

genesisHeavyDelThd :: CoinPortion
genesisHeavyDelThd = bvdHeavyDelThd genesisBlockVersionData

genesisUpdateVoteThd :: CoinPortion
genesisUpdateVoteThd = bvdUpdateVoteThd genesisBlockVersionData

genesisSlotDuration :: Millisecond
genesisSlotDuration = bvdSlotDuration genesisBlockVersionData

genesisMaxBlockSize :: Byte
genesisMaxBlockSize = bvdMaxBlockSize genesisBlockVersionData

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
