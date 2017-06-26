{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Core.Genesis
       (
       -- * /genesis-core.bin/
         StakeDistribution(..)
       , GenesisCoreData(..)
       , compileGenCoreData
       -- ** Derived data
       , genesisAddresses
       , genesisStakeDistribution
       , genesisBootBalances
       , genesisBootStakeholders

       -- * Constants
       , genesisDevKeyPairs
       , genesisDevPublicKeys
       , genesisDevSecretKeys
       , genesisDevHdwSecretKeys

       -- * Utils
       , generateGenesisKeyPair
       , generateHdwGenesisSecretKey
       , getTotalStake
       , genesisSplitBoot
       ) where

import           Universum

import           Control.Lens            (_head)
import           Data.Default            (def)
import qualified Data.HashSet            as HS
import qualified Data.Text               as T
import           Formatting              (int, sformat, (%))

import           Pos.Binary.Crypto       ()
import           Pos.Core.Address        (makePubKeyAddress)
import           Pos.Core.Coin           (unsafeAddCoin)
import           Pos.Core.Constants      (genesisKeysN, isDevelopment)
import           Pos.Core.Genesis.Parser (compileGenCoreData)
import           Pos.Core.Genesis.Types  (GenesisCoreData (..), StakeDistribution (..),
                                          getTotalStake)
import           Pos.Core.Types          (Address, Coin, StakeholderId, mkCoin,
                                          unsafeGetCoin)
import           Pos.Crypto.SafeSigning  (EncryptedSecretKey, emptyPassphrase,
                                          safeDeterministicKeyGen)
import           Pos.Crypto.Signing      (PublicKey, SecretKey, deterministicKeyGen)
import           Pos.Util.Util           (getKeys)

----------------------------------------------------------------------------
-- Constants
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

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

generateGenesisKeyPair :: Int -> (PublicKey, SecretKey)
generateGenesisKeyPair =
    fromMaybe (error "deterministicKeyGen failed in Genesis") .
    deterministicKeyGen .
    encodeUtf8 .
    T.take 32 . sformat ("My awesome 32-byte seed #" %int % "             ")

generateHdwGenesisSecretKey :: Int -> EncryptedSecretKey
generateHdwGenesisSecretKey =
    snd .
    fromMaybe (error "safeDeterministicKeyGen failed in Genesis") .
    flip safeDeterministicKeyGen emptyPassphrase .
    encodeUtf8 .
    T.take 32 . sformat ("My 32-byte hdw seed #" %int % "                  ")

----------------------------------------------------------------------------
-- GenesisCore-derived data
----------------------------------------------------------------------------

-- | List of addresses in genesis.
genesisAddresses :: [Address]
genesisAddresses
    | isDevelopment = map makePubKeyAddress genesisDevPublicKeys
    | otherwise     = gcdAddresses compileGenCoreData

genesisStakeDistribution :: StakeDistribution
genesisStakeDistribution
    | isDevelopment = def
    | otherwise     = gcdDistribution compileGenCoreData

-- | List of bootstrap balances with coins.
genesisBootBalances :: HashMap StakeholderId Coin
genesisBootBalances
    | isDevelopment = mempty
    | otherwise     = gcdBootstrapBalances compileGenCoreData

-- | Bootstrap era stakeholders.
genesisBootStakeholders :: HashSet StakeholderId
genesisBootStakeholders = getKeys genesisBootBalances

-- | Returns a distribution sharing coins to
-- 'genesisBootStakeholders'.  If number of addresses is less then
-- coins, we give 1 coin to prefix.  Otherwise we give quotient to
-- everyone and remainder to the first one.
genesisSplitBoot :: Coin -> [(StakeholderId, Coin)]
genesisSplitBoot c
    | cval <= 0 =
      error $ "sendMoney#splitBoot: cval <= 0: " <> show cval
    | cval < addrsNum =
      map (,mkCoin 1) $ take cval bootStakeholders
    | otherwise =
      let (d :: Word64, m :: Word64) =
              bimap fromIntegral fromIntegral $
              divMod cval addrsNum
          stakeCoins = replicate addrsNum (mkCoin d) &
                           _head %~ unsafeAddCoin (mkCoin m)
      in bootStakeholders `zip` stakeCoins
  where
    cval :: Int
    cval = fromIntegral $ unsafeGetCoin c
    addrsNum :: Int
    addrsNum = length genesisBootStakeholders
    bootStakeholders = HS.toList genesisBootStakeholders
