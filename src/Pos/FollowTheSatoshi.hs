{-# LANGUAGE MultiWayIf #-}

-- | Everything related to /follow-the-satoshi/ procedure.

module Pos.FollowTheSatoshi
       ( FtsError(..)
       , calculateSeed
       , followTheSatoshi
       ) where

import qualified Data.ByteString     as BS (pack, zipWith)
import qualified Data.HashMap.Strict as HM (fromList, lookup, mapMaybe, toList)
import qualified Data.HashSet        as HS (difference, fromMap)
import           Data.List           (foldl1', scanl1)
import qualified Data.Map.Strict     as M
import           Universum

import           Pos.Constants       (epochSlots)
import           Pos.Crypto          (PublicKey, Secret (..), Signed (..), deterministic,
                                      randomNumber, recoverSecret, verifyProof)
import           Pos.Types           (Address, Coin (..), Commitment (..), CommitmentsMap,
                                      FtsSeed (..), OpeningsMap, SharesMap, Utxo,
                                      getOpening)

data FtsError
    -- | Some nodes in the 'OpeningsMap' aren't in the set of participants
    = ExtraneousOpenings (HashSet PublicKey)
    -- | Some nodes in the 'SharesMap' aren't in the set of participants
    | ExtraneousShares (HashSet PublicKey)
    -- | There were no participants so a random string couldn't be generated
    | NoParticipants
    -- | Commitment didn't match secret (either recovered or in openings)
    | BrokenCommitment PublicKey
    -- | Secret couldn't be recovered, or wasn't found in either
    -- 'OpeningsMap' or 'SharesMap'
    | NoSecretFound PublicKey
    deriving (Eq, Show)

getKeys :: HashMap k v -> HashSet k
getKeys = HS.fromMap . void

-- | Calculate rho. Rho is a random bytestring that all nodes generate
-- together and agree on.
--
-- TODO: do we need to check secrets' lengths? Probably not.
calculateSeed
    :: CommitmentsMap        -- ^ All participating nodes
    -> OpeningsMap
    -> SharesMap
    -> Either FtsError FtsSeed
calculateSeed commitments openings shares = do
    let participants = getKeys commitments

    -- First let's do some sanity checks.
    let extraOpenings, extraShares :: HashSet PublicKey
        extraOpenings = HS.difference (getKeys openings) participants
        extraShares =
            let xs = getKeys shares <>
                     mconcat (map (getKeys . signedValue) (toList shares))
            in  HS.difference xs participants
    unless (null extraOpenings) $
        Left (ExtraneousOpenings extraOpenings)
    unless (null extraShares) $
        Left (ExtraneousShares extraShares)

    -- Then we can start calculating seed, but first we have to recover some
    -- secrets (if corresponding openings weren't posted)

    -- Participants for whom we have to recover the secret
    let mustBeRecovered :: HashSet PublicKey
        mustBeRecovered = HS.difference participants (getKeys openings)
    -- Secrets recovered from actual share lists (but only those we need –
    -- i.e. ones which are in mustBeRecovered)
    let recovered :: HashMap PublicKey (Maybe Secret)
        recovered = HM.fromList $ do
            -- We are now trying to recover a secret for key 'k'
            k <- toList mustBeRecovered
            -- We collect all secrets that 'k' has sent to other nodes
            let secrets = mapMaybe (HM.lookup k . signedValue) (toList shares)
            -- Then we try to recover the secret
            return (k, recoverSecret secrets)

    -- All secrets, both recovered and from openings
    let openingToSecret = Secret . getFtsSeed . getOpening
    let secrets :: HashMap PublicKey Secret
        secrets = fmap (openingToSecret . signedValue) openings <>
                  HM.mapMaybe identity recovered

    -- Now that we have the secrets, we can check whether the commitments
    -- actually match the secrets, and whether a secret has been recovered
    -- for each participant.
    for_ (HM.toList commitments) $ \(key, commitment) -> do
        secret <- case HM.lookup key secrets of
            Nothing -> Left (NoSecretFound key)
            Just sc -> return sc
        unless (verifyProof (commProof (signedValue commitment)) secret) $
            Left (BrokenCommitment key)

    -- Finally we just XOR all secrets together
    let xorBS a b = BS.pack (BS.zipWith xor a b)  -- fast due to rewrite rules
    if | null secrets && not (null participants) ->
             panic "calculateSeed: there were some participants \
                   \but they produced no secrets somehow"
       | null secrets -> Left NoParticipants
       | otherwise    -> Right $
                         FtsSeed $ foldl1' xorBS (map getSecret (toList secrets))

-- | Choose several random stakeholders (specifically, their amount is
-- currently hardcoded in 'Pos.Constants.epochSlots').
--
-- The probability that a stakeholder will be chosen is proportional to the
-- number of coins this stakeholder holds. The same stakeholder can be picked
-- more than once.
--
-- How the algorithm works: wWe sort all unspent outputs in a deterministic
-- way (lexicographically) and have an ordered sequence of pairs @(Address,
-- Coin)@. Then we choose several random 'i's between 1 and amount of satoshi
-- in the system; to find owner of 'i'th coin we find the lowest x such that
-- sum of all coins in this list up to 'i'th is not less than 'i' (and then
-- 'x'th address is the owner).
followTheSatoshi :: FtsSeed -> Utxo -> [Address]
followTheSatoshi (FtsSeed seed) utxo
    | null outputs = panic "followTheSatoshi: utxo is empty"
    | otherwise    = map fst $ sortOn snd $
                     findLeaders (sortOn fst $ zip coinIndices [1..]) sums
  where
    outputs :: [(Address, Coin)]
    outputs = [(addr, coin) | ((_, _, coin), addr) <- M.toList utxo]

    -- TODO: not sure that 'sum' will use strict foldl' here, because 'sum'
    -- is only specialised for some types
    totalCoins :: Coin
    totalCoins = sum (map snd outputs)

    coinIndices :: [Coin]
    coinIndices = map (fromInteger . (+1)) $
                  deterministic seed $
                  replicateM epochSlots (randomNumber (toInteger totalCoins))

    sums :: [(Address, Coin)]
    sums = scanl1 (\(_,c1) (a,c2) -> (a, c1 + c2)) outputs

    -- The coin indices have to be sorted by amount, but we want to produce
    -- addresses in the same order as 'secureRandomNumbers' produced the coin
    -- indices. To achieve this, we sort the indices by amount but leave the
    -- original indices-of-coin-indices. Later we'll sort addresses by
    -- original indices and thus restore the order.
    findLeaders :: [(Coin, Int)] -> [(Address, Coin)] -> [(Address, Int)]
    findLeaders [] _ = []
    findLeaders _ [] = panic "followTheSatoshi: indices out of range"
    findLeaders ((c,ci):cs) ((a,x):xs)
        | x >= c    = (a,ci) : findLeaders cs ((a,x):xs)
        | otherwise = findLeaders ((c,ci):cs) xs
