{-# LANGUAGE MultiWayIf   #-}
{-# LANGUAGE ViewPatterns #-}

-- | Actual shared seed calculation.

module Pos.Ssc.GodTossing.Seed
       ( calculateSeed
       ) where

import           Control.Arrow            ((&&&))
import qualified Data.HashMap.Strict      as HM (fromList, lookup, mapMaybe, toList)
import qualified Data.HashSet             as HS (difference, fromMap)
import           Universum

import           Pos.Crypto               (PublicKey, Secret, Share, Threshold, shareId,
                                           unsafeRecoverSecret)
import           Pos.Ssc.GodTossing.Base  (CommitmentsMap, OpeningsMap, SharesMap,
                                           getOpening, secretToSharedSeed, verifyOpening)
import           Pos.Ssc.GodTossing.Error (SeedError (..))
import           Pos.Types                (SharedSeed)

getKeys :: HashMap k v -> HashSet k
getKeys = HS.fromMap . void

-- | Calculate SharedSeed. SharedSeed is a random bytestring that all
-- nodes generate together and agree on.
--
-- TODO: do we need to check secrets' lengths? Probably not.
calculateSeed
    :: Threshold
    -> CommitmentsMap        -- ^ All participating nodes
    -> OpeningsMap
    -> SharesMap             -- ^ Decrypted shares
    -> Either SeedError SharedSeed
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
    -- TODO: see CSL-50
    -- for_ (HM.toList commitments) $ \(key, fst -> _) -> do
    --     case HM.lookup key secrets of
    --         Nothing -> Left (NoSecretFound key)
    --         Just _  -> pure ()

    -- Finally we just XOR all secrets together
    if | null secrets && not (null participants) ->
             panic "calculateSeed: there were some participants \
                   \but they produced no secrets somehow"
       | null secrets -> Left NoParticipants
       | otherwise    -> Right $
                         mconcat $ map secretToSharedSeed (toList secrets)
