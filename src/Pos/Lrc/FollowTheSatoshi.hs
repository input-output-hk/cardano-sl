{-# LANGUAGE ScopedTypeVariables #-}

-- | Everything related to /follow-the-satoshi/ procedure.

module Pos.Lrc.FollowTheSatoshi
       ( followTheSatoshi
       , followTheSatoshiM
       ) where

import qualified Data.HashMap.Strict as HM
import           Data.List.NonEmpty  (fromList)
import           Universum

import           Pos.Constants       (epochSlots)
import           Pos.Crypto          (deterministic, randomNumber)
import           Pos.Txp.Core.Types  (Utxo)
import           Pos.Txp.Toil.Utxo   (utxoToStakes)
import           Pos.Types           (Coin, SharedSeed (..), StakeholderId, coinToInteger,
                                      mkCoin, sumCoins, unsafeAddCoin)
import           Pos.Util.Iterator   (MonadIterator (..), runListHolder)

-- | A version of 'followTheSatoshi' that uses an iterator over 'TxOut's
-- instead of 'Utxo'.
followTheSatoshiM
    :: forall m . MonadIterator (StakeholderId, Coin) m
    => SharedSeed -> Coin -> m (NonEmpty StakeholderId)
followTheSatoshiM _ totalCoins
    | totalCoins == mkCoin 0 = panic "followTheSatoshiM: nobody has any stake"
followTheSatoshiM (SharedSeed seed) totalCoins = do
    res <- findLeaders (sortOn fst $ zip coinIndices [1..]) (mkCoin 0)
    pure . fromList . map fst . sortOn snd $ res
  where
    coinIndices :: [Coin]
    -- There won't be overflow because totalCoins fits in 64 bits
    coinIndices = map (mkCoin . fromInteger . (+1)) $
              deterministic seed $
              replicateM epochSlots (randomNumber (coinToInteger totalCoins))

    findLeaders
        :: [(Coin, Int)]
        -> Coin
        -> m [(StakeholderId, Int)]
    -- We found all coins we wanted to find
    findLeaders [] _ = pure []
    -- We ran out of items in the buffer so we take a new output
    -- and refill the buffer
    findLeaders coins sm = nextItem @(StakeholderId, Coin) >>=
        maybe (panic "followTheSatoshiM: indices out of range") (onItem coins)
      where
        -- We check whether `c` is covered by current item in the buffer
        onItem [] _ = pure []
        onItem (c:cs) it@(adr, val)
            | sm' >= fst c = ((adr, snd c):) <$> onItem cs it
            | otherwise = findLeaders (c:cs) sm'
          where
            sm' = unsafeAddCoin sm val

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
          panic "followTheSatoshi: utxo is empty"
    | totalCoins > coinToInteger (maxBound @Coin) =
          panic "followTheSatoshi: totalCoins exceeds Word64"
    | otherwise =
          runListHolder
              (followTheSatoshiM seed
                   (mkCoin (fromInteger totalCoins)))
              stakes
  where
    stakes = HM.toList $ utxoToStakes utxo
    totalCoins = sumCoins $ map snd stakes
