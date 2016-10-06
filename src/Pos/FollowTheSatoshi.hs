{-# LANGUAGE MultiWayIf #-}

module Pos.FollowTheSatoshi
       ( Rho(..)
       , RhoError(..)
       , calculateRho
       , followTheSatoshi
       ) where

import qualified Data.ByteString     as BS (pack, zipWith)
import qualified Data.HashMap.Strict as HM (filterWithKey, lookup, mapMaybe, toList)
import qualified Data.HashSet        as HS (difference, fromMap, member)
import           Data.List           (foldl1', scanl1)
import           Universum

import           Pos.Crypto          (PublicKey, Secret (..), recoverSecret,
                                      secureRandomNumber, verifyProof)
import           Pos.Types           (Address, Coin (..), Commitment (..), CommitmentsMap,
                                      OpeningsMap, SharesMap, Utxo, getOpening)

-- | A random shared bytestring that all nodes agree on. Must be 40 bytes
-- long (since it's going to be used as input for the ChaCha random number
-- generator, which uses a 40-byte seed).
newtype Rho = Rho ByteString

data RhoError
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

getKeys :: HashMap k v -> HashSet k
getKeys = HS.fromMap . void

-- | Calculate rho. Rho is a random bytestring that all nodes generate
-- together and agree on.
--
-- TODO: do we need to check secrets' lengths? Probably not.
calculateRho
    :: CommitmentsMap        -- ^ All participating nodes
    -> OpeningsMap
    -> SharesMap
    -> Either RhoError Rho
calculateRho commitments openings shares = do
    let participants = getKeys commitments

    -- First let's do some sanity checks.
    let extraOpenings, extraShares :: HashSet PublicKey
        extraOpenings = HS.difference (getKeys openings) participants
        extraShares =
            let xs = getKeys shares <> mconcat (map getKeys (toList shares))
            in  HS.difference xs participants
    unless (null extraOpenings) $
        Left (ExtraneousOpenings extraOpenings)
    unless (null extraShares) $
        Left (ExtraneousShares extraShares)

    -- Then we can start calculating rho, but first we have to recover some
    -- secrets (if corresponding openings weren't posted)

    -- Participants for whom we have to recover the secret
    let mustBeRecovered :: HashSet PublicKey
        mustBeRecovered = HS.difference participants (getKeys openings)
    -- Secrets recovered from actual share lists (but only those we need –
    -- i.e. ones which are in mustBeRecovered)
    let recovered :: HashMap PublicKey (Maybe Secret)
        recovered = fmap (recoverSecret . toList) $
            HM.filterWithKey (\k _ -> k `HS.member` mustBeRecovered) shares
    -- All secrets, both recovered and from openings
    let secrets :: HashMap PublicKey Secret
        secrets = fmap getOpening openings <> HM.mapMaybe identity recovered

    -- Now that we have the secrets, we can check whether the commitments
    -- actually match the secrets, and whether a secret has been recovered
    -- for each participant.
    for_ (HM.toList commitments) $ \(key, commitment) -> do
        secret <- case HM.lookup key secrets of
            Nothing -> Left (NoSecretFound key)
            Just sc -> return sc
        unless (verifyProof (commProof commitment) secret) $
            Left (BrokenCommitment key)

    -- Finally we just XOR all secrets together
    let xorBS a b = BS.pack (BS.zipWith xor a b)  -- fast due to rewrite rules
    if | null secrets && not (null participants) ->
             panic "calculateRho: there were some participants \
                   \but they produced no secrets somehow"
       | null secrets -> Left NoParticipants
       | otherwise    -> Right $
                         Rho $ foldl1' xorBS (map getSecret (toList secrets))

-- | Choose a random stakeholder. The probability that a stakeholder will be
-- chosen is proportional to the number of coins this stakeholder holds.
--
-- We sort all unspent outputs in a deterministic way (lexicographically) and
-- have an ordered sequence of pairs @(Address, Coin)@. We choose a random i
-- between 1 and amount of satoshi in the system; to find owner of i-th coin
-- we find the lowest x such that sum of all coins in this list up to i-th is
-- not less than i (and then x-th address is the owner).
followTheSatoshi :: Rho -> Utxo -> Maybe Address
followTheSatoshi (Rho seed) utxo
    | null outputs = Nothing
    | otherwise    = fmap fst $ find ((>= coinIndex) . snd) $
                     scanl1 (\(_,c1) (a,c2) -> (a, c1+c2)) outputs
  where
    outputs :: [(Address, Coin)]
    outputs = sort [(addr, coin) | ((_, _, coin), addr) <- HM.toList utxo]
    -- TODO: not sure that 'sum' will use strict foldl' here, because 'sum'
    -- is only specialised for some types
    totalCoins, coinIndex :: Coin
    totalCoins = sum (map snd outputs)
    coinIndex = fromInteger $
                1 + secureRandomNumber seed (toInteger totalCoins)
