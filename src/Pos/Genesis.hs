{-| Blockchain genesis. Not to be confused with genesis block in epoch.
    Blockchain genesis means genesis values which are hardcoded in advance
    (before system starts doing anything). Genesis block in epoch exists
    in every epoch and it's not known in advance.
-}

module Pos.Genesis
       (
       -- * Static state
         StakeDistribution (..)
       , genesisAddresses
       , genesisKeyPairs
       , genesisPublicKeys
       , genesisSecretKeys
       , genesisUtxo

       -- * Ssc
       , genesisLeaders
       ) where


import           Data.Default         (Default (def))
import           Data.List            (genericLength, genericReplicate)
import qualified Data.Map.Strict      as M
import qualified Data.Text            as T
import           Formatting           (int, sformat, (%))
import           Serokell.Util        (enumerate)
import           Universum

import           Pos.Constants        (genesisN)
import           Pos.Crypto           (PublicKey, SecretKey, deterministicKeyGen,
                                       unsafeHash)
import           Pos.FollowTheSatoshi (followTheSatoshi)
import           Pos.Types            (Address (..), Coin, SharedSeed (SharedSeed),
                                       SlotLeaders, TxOut (..), Utxo)


----------------------------------------------------------------------------
-- Static state
----------------------------------------------------------------------------

-- | List of pairs from 'SecretKey' with corresponding 'PublicKey'.
genesisKeyPairs :: [(PublicKey, SecretKey)]
genesisKeyPairs = map gen [0 .. genesisN - 1]
  where
    gen :: Int -> (PublicKey, SecretKey)
    gen =
        fromMaybe (panic "deterministicKeyGen failed in Genesis") .
        deterministicKeyGen .
        encodeUtf8 .
        T.take 32 . sformat ("My awesome 32-byte seed #" %int % "             ")

-- | List of 'SecrekKey'`s in genesis.
genesisSecretKeys :: [SecretKey]
genesisSecretKeys = map snd genesisKeyPairs

-- | List of 'PublicKey'`s in genesis.
genesisPublicKeys :: [PublicKey]
genesisPublicKeys = map fst genesisKeyPairs

-- | List of 'Address'`es in genesis. See 'genesisPublicKeys'.
genesisAddresses :: [Address]
genesisAddresses = map Address genesisPublicKeys

-- | Stake distribution in genesis block.
-- FlatStakes is a flat distribution, i. e. each node has the same amount of coins.
-- BitcoinStakes is a Bitcoin mining pool-style ditribution.
data StakeDistribution
    = FlatStakes !Word     -- number of stakeholders
                 !Coin     -- total number of coins
    | BitcoinStakes !Word  -- number of stakeholders
                    !Coin  -- total number of coins

instance Default StakeDistribution where
    def = FlatStakes genesisN (genesisN * 10000)

bitcoinDistribution20 :: [Coin]
bitcoinDistribution20 = [200, 163, 120, 105, 78, 76, 57, 50, 46, 31, 26, 13, 11, 11, 7, 4, 2, 0, 0, 0]

stakeDistribution :: StakeDistribution -> [Coin]
stakeDistribution (FlatStakes stakeholders coins) =
    genericReplicate stakeholders val
  where
    val = coins `div` fromIntegral stakeholders
stakeDistribution (BitcoinStakes stakeholders coins) =
    map normalize $ bitcoinDistribution1000Coins stakeholders
  where
    normalize x = x * coins `div` 1000

bitcoinDistribution1000Coins :: Word -> [Coin]
bitcoinDistribution1000Coins stakeholders
    | stakeholders < 20 = stakeDistribution (FlatStakes stakeholders 1000)
    | stakeholders == 20 = bitcoinDistribution20
    | otherwise =
        foldl' (bitcoinDistributionImpl ratio) [] $
        enumerate bitcoinDistribution20
  where
    ratio = fromIntegral stakeholders / 20

bitcoinDistributionImpl :: Double -> [Coin] -> (Int, Coin) -> [Coin]
bitcoinDistributionImpl ratio coins (coinIdx, coin) =
    coins ++ toAddValMax : replicate (toAddNum - 1) toAddValMin
  where
    toAddNumMax = ceiling ratio
    toAddNumMin = floor ratio
    toAddNum :: Int
    toAddNum =
        if genericLength coins + realToFrac toAddNumMax >
           realToFrac (coinIdx + 1) * ratio
            then toAddNumMin
            else toAddNumMax
    toAddValMin = coin `div` fromIntegral toAddNum
    toAddValMax = coin - toAddValMin * (fromIntegral toAddNum - 1)

-- | Genesis 'Utxo'.
genesisUtxo :: StakeDistribution -> Utxo
genesisUtxo sd =
    M.fromList . zipWith zipF (stakeDistribution sd) $ genesisAddresses
  where
    zipF coin addr = ((unsafeHash addr, 0), TxOut addr coin)

----------------------------------------------------------------------------
-- Slot leaders
----------------------------------------------------------------------------

genesisSeed :: SharedSeed
genesisSeed = SharedSeed "vasa opasa skovoroda Ggurda boroda provoda"

-- | Leaders of genesis. See 'followTheSatoshi'.
genesisLeaders :: Utxo -> SlotLeaders
genesisLeaders = fmap getAddress . followTheSatoshi genesisSeed
