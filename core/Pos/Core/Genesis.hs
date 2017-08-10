{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Core.Genesis
       (
       -- * Constants/devmode
         genesisDevKeyPairs
       , genesisDevPublicKeys
       , genesisDevSecretKeys
       , genesisDevHdwSecretKeys
       , genesisDevFlatDistr

       -- * /genesis-core.bin/
       , module Pos.Core.Genesis.Types
       , compileGenCoreData

       -- ** Derived data
       , genesisProdAddresses
       , genesisProdAddrDistribution
       , genesisProdBootStakeholders

       -- * Utils
       , generateGenesisKeyPair
       , generateHdwGenesisSecretKey
       , genesisSplitBoot
       ) where

import           Universum

import           Control.Lens            (ix)
import           Data.Hashable           (hash)
import qualified Data.HashMap.Strict     as HM
import qualified Data.Text               as T
import           Formatting              (int, sformat, (%))

import           Pos.Binary.Crypto       ()
import           Pos.Core.Coin           (unsafeAddCoin, unsafeMulCoin)
import           Pos.Core.Constants      (genesisKeysN)
import           Pos.Core.Genesis.Parser (compileGenCoreData)
import           Pos.Core.Genesis.Types  (AddrDistribution, GenesisCoreData (..),
                                          GenesisWStakeholders (..),
                                          StakeDistribution (..), bootDustThreshold,
                                          getTotalStake, mkGenesisCoreData, safeExpStakes)
import           Pos.Core.Types          (Address, Coin, StakeholderId, mkCoin,
                                          unsafeGetCoin)
import           Pos.Crypto.SafeSigning  (EncryptedSecretKey, emptyPassphrase,
                                          safeDeterministicKeyGen)
import           Pos.Crypto.Signing      (PublicKey, SecretKey, deterministicKeyGen)

----------------------------------------------------------------------------
-- Constants/development
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
genesisDevFlatDistr :: StakeDistribution
genesisDevFlatDistr =
    FlatStakes genesisKeysN $
    mkCoin 10000 `unsafeMulCoin` (genesisKeysN :: Int)

----------------------------------------------------------------------------
-- GenesisCore derived data, production
----------------------------------------------------------------------------

-- | List of addresses in genesis binary file.
genesisProdAddresses :: [Address]
genesisProdAddresses =
    concatMap (toList . fst) $ gcdAddrDistribution compileGenCoreData

-- | Address and distribution set for production mode.
genesisProdAddrDistribution :: [AddrDistribution]
genesisProdAddrDistribution = gcdAddrDistribution compileGenCoreData

-- | Bootstrap era stakeholders for production mode.
genesisProdBootStakeholders :: GenesisWStakeholders
genesisProdBootStakeholders =
    gcdBootstrapStakeholders compileGenCoreData

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

-- | Returns a distribution sharing coins to given boot
-- stakeholders. Number of stakeholders must be less or equal to the
-- related 'bootDustThreshold'. Function splits coin on chunks of
-- @weightsSum@ and distributes it over boot stakeholder. Remainder is
-- assigned randomly (among boot stakeholders) based on hash of the coin.
--
-- If coin is lower than 'bootDustThreshold' then this function
-- distributes coins among first stakeholders in the list according to
-- their weights.
genesisSplitBoot ::
       GenesisWStakeholders -> Coin -> [(StakeholderId, Coin)]
genesisSplitBoot g@(GenesisWStakeholders bootWHolders) c
    | c < bootDustThreshold g =
          snd $ foldr foldrFunc (0::Word64,[]) (HM.toList bootWHolders)
    | otherwise =
          bootHolders `zip` stakeCoins
  where
    foldrFunc (s,w) r@(totalSum, res) = case compare totalSum cval of
        EQ -> r
        GT -> error "genesisSplitBoot: totalSum > cval can't happen"
        LT -> let w' = (fromIntegral w :: Word64)
                  toInclude = bool w' (cval - totalSum) (totalSum + w' > cval)
              in (totalSum + toInclude
                 ,(s, mkCoin toInclude):res)

    weights :: [Word64]
    weights = map fromIntegral $ HM.elems bootWHolders

    weightsSum :: Word64
    weightsSum = sum weights

    bootHolders :: [StakeholderId]
    bootHolders = HM.keys bootWHolders

    (d :: Word64, m :: Word64) = divMod cval weightsSum

    -- Person who will get the remainder in (2) case.
    remReceiver :: Int
    remReceiver = abs (hash c) `mod` addrsNum

    cval :: Word64
    cval = unsafeGetCoin c

    addrsNum :: Int
    addrsNum = length bootHolders

    stakeCoins :: [Coin]
    stakeCoins =
        map (\w -> mkCoin $ w * d) weights &
        ix remReceiver %~ unsafeAddCoin (mkCoin m)
