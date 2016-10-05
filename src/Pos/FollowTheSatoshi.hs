{-# LANGUAGE MultiWayIf #-}

module Pos.FollowTheSatoshi
       ( Rho(..)
       , RhoError(..)
       , calculateRho
       ) where

import qualified Data.ByteString     as BS (pack, zipWith)
import qualified Data.HashMap.Strict as HM (filterWithKey, lookup, mapMaybe, toList)
import qualified Data.HashSet        as HS (difference, fromMap, member)
import           Data.List           (foldl1')
import           Universum

import           Pos.Crypto          (PublicKey, Secret (..), recoverSecret, verifyProof)
import           Pos.Types           (Commitment (..), CommitmentsMap, OpeningsMap,
                                      SharesMap, getOpening)

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
