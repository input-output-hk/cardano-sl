-- | Blockchain genesis. Not to be confused with genesis block in epoch.

module Pos.Genesis
       (
       -- * Static state
         StakeDistribution (..)
       , genesisAddresses
       , genesisKeyPairs
       , genesisPublicKeys
       , genesisSecretKeys
       , genesisUtxo
       , genesisUtxoPetty

       -- * Ssc
       , genesisLeaders
       ) where


import           Data.Default       (Default (def))
import           Data.List          (genericLength, genericReplicate, (!!))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict    as M
import qualified Data.Text          as T
import           Formatting         (int, sformat, (%))
import           Serokell.Util      (enumerate)
import           Universum

import           Pos.Constants      (epochSlots, genesisN)
import           Pos.Crypto         (PublicKey, SecretKey, deterministicKeyGen,
                                     unsafeHash)
import           Pos.Types.Types    (Address (Address), Coin, SlotLeaders, TxOut (..),
                                     Utxo)


----------------------------------------------------------------------------
-- Static state
----------------------------------------------------------------------------

-- TODO get rid of this hardcode !!
-- Secret keys of genesis block participants shouldn't obviously be widely known
genesisKeyPairs :: [(PublicKey, SecretKey)]
genesisKeyPairs = map gen [0 .. genesisN - 1]
  where
    gen :: Int -> (PublicKey, SecretKey)
    gen =
        fromMaybe (panic "deterministicKeyGen failed in Genesis") .
        deterministicKeyGen .
        encodeUtf8 .
        T.take 32 . sformat ("My awesome 32-byte seed #" %int % "             ")

genesisSecretKeys :: [SecretKey]
genesisSecretKeys = map snd genesisKeyPairs

genesisPublicKeys :: [PublicKey]
genesisPublicKeys = map fst genesisKeyPairs

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

sdStakeHolders :: StakeDistribution -> Word
sdStakeHolders (FlatStakes n _)    = n
sdStakeHolders (BitcoinStakes n _) = n

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

genesisUtxo :: StakeDistribution -> Utxo
genesisUtxo sd =
    M.fromList . zipWith zipF (stakeDistribution sd) $ genesisAddresses
  where
    zipF coin addr = ((unsafeHash addr, 0), TxOut addr coin)

-- | Each utxo is split into many utxos, each containing 1 coin. Only
-- for 0-th node.
genesisUtxoPetty :: StakeDistribution -> Utxo
genesisUtxoPetty sd =
    M.fromList $
    flip concatMap (genesisAddresses `zip` [0 .. sdStakeHolders sd - 1]) $
    \(a, nodei) ->
         let c = coinsDistr !! fromIntegral nodei
         in if nodei == 0
                then map
                         (\i -> ((unsafeHash (show a ++ show i), 0), TxOut a 1))
                         [1 .. fromIntegral c :: Int]
                else [((unsafeHash a, 0), TxOut a c)]
  where
    coinsDistr = stakeDistribution sd

----------------------------------------------------------------------------
-- Slot leaders
----------------------------------------------------------------------------

genesisLeaders :: SlotLeaders
genesisLeaders = NE.fromList $ replicate epochSlots pk
  where
    pk =
        fromMaybe (panic "genesisPublicKeys is empty") $
        headMay genesisPublicKeys
