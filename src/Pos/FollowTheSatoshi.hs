{-# LANGUAGE MultiWayIf   #-}
{-# LANGUAGE ViewPatterns #-}

-- | Everything related to /follow-the-satoshi/ procedure.

module Pos.FollowTheSatoshi
       ( FtsError(..)
       , calculateSeed
       , followTheSatoshi
       ) where

import           Control.Arrow       ((&&&))
import qualified Data.HashMap.Strict as HM (fromList, lookup, mapMaybe, toList)
import qualified Data.HashSet        as HS (difference, fromMap)
import           Data.List           (foldl1', scanl1)
import           Data.Text.Buildable (Buildable (..))
import           Universum

import           Serokell.Util       (listBuilderJSON)

import           Pos.Constants       (epochSlots)
import           Pos.Crypto          (PublicKey, Secret, Share, Threshold, deterministic,
                                      randomNumber, shareId, unsafeRecoverSecret)
import           Pos.Types           (Address, Coin (..), CommitmentsMap, FtsSeed (..),
                                      OpeningsMap, SharesMap, TxOut (..), Utxo,
                                      getOpening, secretToFtsSeed, verifyOpening,
                                      xorFtsSeed)

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

instance Buildable FtsError where
    build (ExtraneousOpenings ks) =
        "ExtraneousOpenings " <> listBuilderJSON ks
    build (ExtraneousShares ks) =
        "ExtraneousShares " <> listBuilderJSON ks
    build NoParticipants =
        "NoParticipants"
    build (BrokenCommitment k) =
        "BrokenCommitment " <> build k
    build (NoSecretFound k) =
        "NoSecretFound " <> build k

getKeys :: HashMap k v -> HashSet k
getKeys = HS.fromMap . void

-- | Calculate rho. Rho is a random bytestring that all nodes generate
-- together and agree on.
--
-- TODO: do we need to check secrets' lengths? Probably not.
calculateSeed
    :: Threshold
    -> CommitmentsMap        -- ^ All participating nodes
    -> OpeningsMap
    -> SharesMap             -- ^ Decrypted shares
    -> Either FtsError FtsSeed
calculateSeed (fromIntegral -> t) commitments openings shares = do
    let participants = getKeys commitments

    -- First let's do some sanity checks.
    let extraOpenings, extraShares :: HashSet PublicKey
        extraOpenings = HS.difference (getKeys openings) participants
        extraShares =
            let xs = getKeys shares <>
                     mconcat (map getKeys (toList shares))
            in  HS.difference xs participants
    unless (null extraOpenings) $
        Left (ExtraneousOpenings extraOpenings)
    unless (null extraShares) $
        Left (ExtraneousShares extraShares)

    -- And let's check openings.
    for_ (HM.toList commitments) $ \(key, fst -> commitment) -> do
        whenJust (HM.lookup key openings) $ \opening ->
            unless (verifyOpening commitment opening) $
                Left (BrokenCommitment key)

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
            -- We collect all secrets that 'k' has sent to other nodes (and
            -- remove shares with equal IDs)
            --
            -- TODO: can we be sure that here different IDs mean different
            -- shares? maybe it'd be better to 'assert' it
            let secrets :: [Share]
                secrets = toList . HM.fromList $
                          map (shareId &&& identity) $
                          mapMaybe (HM.lookup k) (toList shares)
            -- Then we recover the secret
            return (k, if length secrets < t
                         then Nothing
                         else Just $ unsafeRecoverSecret (take t secrets))

    -- All secrets, both recovered and from openings
    let secrets :: HashMap PublicKey Secret
        secrets = fmap getOpening openings <>
                  HM.mapMaybe identity recovered

    -- Now that we have the secrets, we can check whether the commitments
    -- actually match the secrets, and whether a secret has been recovered
    -- for each participant.
    for_ (HM.toList commitments) $ \(key, fst -> _) -> do
        case HM.lookup key secrets of
            Nothing -> Left (NoSecretFound key)
            Just _  -> pure ()

    -- Finally we just XOR all secrets together
    if | null secrets && not (null participants) ->
             panic "calculateSeed: there were some participants \
                   \but they produced no secrets somehow"
       | null secrets -> Left NoParticipants
       | otherwise    -> Right $
                         foldl1' xorFtsSeed (map secretToFtsSeed (toList secrets))

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
followTheSatoshi :: FtsSeed -> Utxo -> [Address]
followTheSatoshi (FtsSeed seed) utxo
    | null outputs = panic "followTheSatoshi: utxo is empty"
    | otherwise    = map fst $ sortOn snd $
                     findLeaders (sortOn fst $ zip coinIndices [1..]) sums
  where
    outputs :: [(Address, Coin)]
    outputs = [(txOutAddress, txOutValue) | TxOut{..} <- toList utxo]

    -- TODO: not sure that 'sum' will use strict foldl' here, because 'sum'
    -- is only specialised for some types
    totalCoins :: Coin
    totalCoins = sum (map snd outputs)

    -- FIXME: current version of `deterministic` can't be used here,
    -- because seed is not necessary 40 bytes (it's 32 in fact)
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
