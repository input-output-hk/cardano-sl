{-# LANGUAGE ScopedTypeVariables #-}

-- | Everything related to /follow-the-satoshi/ procedure.

module Pos.Lrc.FtsPure
       ( followTheSatoshi
       , followTheSatoshiM
       ) where

import           Data.Conduit        (runConduitPure, (.|))
import qualified Data.Conduit.List   as CL
import qualified Data.HashMap.Strict as HM
import           Universum

import           Pos.Lrc.Fts         (followTheSatoshiM)
import           Pos.Txp.Toil.Types  (Utxo)
import           Pos.Txp.Toil.Utxo   (utxoToStakes)
import           Pos.Types           (Coin, SharedSeed (..), StakeholderId, coinToInteger,
                                      mkCoin, sumCoins)

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
followTheSatoshi :: SharedSeed -> Utxo -> NonEmpty StakeholderId
followTheSatoshi seed utxo
    | null stakes =
          error "followTheSatoshi: utxo is empty"
    | totalCoins > coinToInteger (maxBound @Coin) =
          error "followTheSatoshi: totalCoins exceeds Word64"
    | otherwise =
          runConduitPure $ CL.sourceList stakes .|
                           followTheSatoshiM seed totalCoinsCoin
  where
    stakes = HM.toList $ utxoToStakes utxo
    totalCoins = sumCoins $ map snd stakes
    totalCoinsCoin = mkCoin $ fromInteger totalCoins
