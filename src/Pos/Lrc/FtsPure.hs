{-# LANGUAGE ScopedTypeVariables #-}

-- | Everything related to /follow-the-satoshi/ procedure.

module Pos.Lrc.FtsPure
       ( followTheSatoshi
       , followTheSatoshiM
       , followTheSatoshiUtxo
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM

import           Pos.Core            (Coin, SharedSeed (..), StakeholderId, coinToInteger,
                                      mkCoin, sumCoins)
import           Pos.Lrc.Fts         (followTheSatoshiM)
import           Pos.Txp.Toil        (Utxo, utxoToStakes)
import           Pos.Util.Iterator   (runListHolder)

-- | Choose several random stakeholders (specifically, their amount is
-- currently hardcoded in 'Pos.Constants.epochSlots').
--
-- The probability that a stakeholder will be chosen is proportional to the
-- number of coins this stakeholder holds. The same stakeholder can be picked
-- more than once.
--
-- How the algorithm works: we sort all unspent outputs in a deterministic
-- way (lexicographically) and have an ordered sequence of pairs
-- @(StakeholderId, Coin)@. Then we choose several random 'i's between 1 and
-- amount of satoshi in the system; to find owner of 'i'th coin we find the
-- lowest x such that sum of all coins in this list up to 'i'th is not less
-- than 'i' (and then 'x'th address is the owner).
--
-- With P2SH addresses, we don't know who is going to end up with funds sent
-- to them. Therefore, P2SH addresses can contain 'addrDestination' which
-- specifies which addresses should count as “owning” funds for the purposes
-- of follow-the-satoshi.
followTheSatoshi :: SharedSeed -> [(StakeholderId, Coin)] -> NonEmpty StakeholderId
followTheSatoshi seed stakes
    | totalCoins > coinToInteger maxBound =
        error "followTheSatoshi: total stake exceeds limit"
    | totalCoinsCoin == minBound = error "followTheSatoshi: no stake"
    | otherwise =
          runListHolder
              (followTheSatoshiM seed
                   totalCoinsCoin)
              stakes
  where
    totalCoins = sumCoins $ map snd stakes
    totalCoinsCoin = mkCoin (fromInteger totalCoins)

followTheSatoshiUtxo :: SharedSeed -> Utxo -> NonEmpty StakeholderId
followTheSatoshiUtxo seed utxo =
    followTheSatoshi seed (HM.toList (utxoToStakes utxo))
