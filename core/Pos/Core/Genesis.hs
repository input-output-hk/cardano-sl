{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Core.Genesis
       (
       -- * Constants
         genesisDevKeyPairs
       , genesisDevPublicKeys
       , genesisDevSecretKeys
       , genesisDevHdwSecretKeys

       -- * /genesis-core.bin/
       , StakeDistribution(..)
       , GenesisCoreData(..)
       , compileGenCoreData
       -- ** Derived data
       , genesisAddresses
       , genesisStakeDistribution
       , genesisBootProdStakeholders

       -- * Utils
       , generateGenesisKeyPair
       , generateHdwGenesisSecretKey
       , getTotalStake
       , genesisSplitBoot
       ) where

import           Universum

import           Control.Lens            (ix)
import           Data.Default            (def)
import           Data.Hashable           (hash)
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
import           Pos.Core.Types          (Address, Coin, StakeholderId, Stakeholders,
                                          mkCoin, unsafeGetCoin)
import           Pos.Crypto.SafeSigning  (EncryptedSecretKey, emptyPassphrase,
                                          safeDeterministicKeyGen)
import           Pos.Crypto.Signing      (PublicKey, SecretKey, deterministicKeyGen)

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

-- | Bootstrap era stakeholders for production mode.
genesisBootProdStakeholders :: HashSet StakeholderId
genesisBootProdStakeholders =
    gcdBootstrapStakeholders compileGenCoreData

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

-- | Returns a distribution sharing coins to given boot
-- stakeholders. If number of addresses @n@ is more than coins @c@, we
-- give 1 coin to every addr in the prefix of length @n@. Otherwise we
-- give quotient to every boot addr and assign remainder randomly
-- based on passed coin hash among addresses.
genesisSplitBoot :: Stakeholders -> Coin -> [(StakeholderId, Coin)]
genesisSplitBoot bootHolders0 c
    | cval <= 0 =
      error $ "sendMoney#splitBoot: cval <= 0: " <> show cval
    | cval < addrsNum =
      map (,mkCoin 1) $ take cval bootHolders
    | otherwise =
      let (d :: Word64, m :: Word64) =
              bimap fromIntegral fromIntegral $
              divMod cval addrsNum
          stakeCoins =
              replicate addrsNum (mkCoin d) &
              ix remReceiver %~ unsafeAddCoin (mkCoin m)
      in bootHolders `zip` stakeCoins
  where
    -- Person who will get the remainder in (2) case.
    remReceiver :: Int
    remReceiver = abs (hash c) `mod` addrsNum
    cval :: Int
    cval = fromIntegral $ unsafeGetCoin c
    addrsNum :: Int
    addrsNum = length bootHolders
    bootHolders :: [StakeholderId]
    bootHolders = HS.toList bootHolders0
