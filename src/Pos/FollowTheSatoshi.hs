{-# LANGUAGE MultiWayIf #-}

-- | Everything related to /follow-the-satoshi/ procedure.

module Pos.FollowTheSatoshi
       ( followTheSatoshi
       ) where




import           Data.List          (scanl1)
import           Data.List.NonEmpty (NonEmpty, fromList)
import           Universum

import           Pos.Constants      (epochSlots)
import           Pos.Crypto         (PublicKey, deterministic, randomNumber)
import           Pos.Types.Address  (Address (..), AddressDestination (..), AddressHash)
import           Pos.Types.Types    (Coin (..), SharedSeed (..), TxOut (..), Utxo)

-- | Choose several random stakeholders (specifically, their amount is
-- currently hardcoded in 'Pos.Constants.epochSlots').
--
-- The probability that a stakeholder will be chosen is proportional to the
-- number of coins this stakeholder holds. The same stakeholder can be picked
-- more than once.
--
-- How the algorithm works: we sort all unspent outputs in a deterministic
-- way (lexicographically) and have an ordered sequence of pairs @(Address,
-- Coin)@. Then we choose several random 'i's between 1 and amount of satoshi
-- in the system; to find owner of 'i'th coin we find the lowest x such that
-- sum of all coins in this list up to 'i'th is not less than 'i' (and then
-- 'x'th address is the owner).
--
-- With P2SH addresses, we don't know who is going to end up with funds sent
-- to them. Therefore, P2SH addresses can contain 'addrDestination' which
-- specifies which addresses should count as “owning” funds for the purposes
-- of follow-the-satoshi.
followTheSatoshi :: SharedSeed -> Utxo -> NonEmpty (AddressHash PublicKey)
followTheSatoshi (SharedSeed seed) utxo
    | null outputs = panic "followTheSatoshi: utxo is empty"
    | otherwise    = fromList $ map fst $ sortOn snd $
                     findLeaders (sortOn fst $ zip coinIndices [1..]) sums
  where
    outputs :: [(AddressHash PublicKey, Coin)]
    outputs = do
        TxOut{..} <- toList utxo
        case addrDestination txOutAddress of
            PubKeyDestination x -> [(x, txOutValue)]
            ScriptDestination _ -> addrDistribution txOutAddress

    totalCoins :: Coin
    totalCoins = sum (map snd outputs)

    coinIndices :: [Coin]
    coinIndices = map (fromInteger . (+1)) $
                  deterministic seed $
                  replicateM epochSlots (randomNumber (toInteger totalCoins))

    sums :: [(AddressHash PublicKey, Coin)]
    sums = scanl1 (\(_,c1) (a,c2) -> (a, c1 + c2)) outputs

    -- The coin indices have to be sorted by amount, but we want to produce
    -- addresses in the same order as 'secureRandomNumbers' produced the coin
    -- indices. To achieve this, we sort the indices by amount but leave the
    -- original indices-of-coin-indices. Later we'll sort addresses by
    -- original indices and thus restore the order.
    findLeaders :: [(Coin, Int)]
                -> [(AddressHash PublicKey, Coin)]
                -> [(AddressHash PublicKey, Int)]
    findLeaders [] _ = []
    findLeaders _ [] = panic "followTheSatoshi: indices out of range"
    findLeaders ((c,ci):cs) ((a,x):xs)
        | x >= c    = (a,ci) : findLeaders cs ((a,x):xs)
        | otherwise = findLeaders ((c,ci):cs) xs
