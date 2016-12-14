{-# LANGUAGE MultiWayIf   #-}
{-# LANGUAGE ViewPatterns #-}

-- | Actual shared seed calculation.

module Pos.Ssc.GodTossing.Seed
       ( calculateSeed
       ) where

import           Control.Arrow                 ((&&&))
import           Control.Lens                  (view, _2)
import qualified Data.HashMap.Strict           as HM (fromList, lookup, mapMaybe, toList,
                                                      traverseWithKey)
import qualified Data.HashSet                  as HS (difference)
import           Universum

import           Pos.Binary.Class              (deserializeM)
import           Pos.Crypto                    (Secret, Share, Threshold, shareId,
                                                unsafeRecoverSecret)
import           Pos.Ssc.GodTossing.Error      (SeedError (..))
import           Pos.Ssc.GodTossing.Functions  (secretToSharedSeed, verifyOpening)
import           Pos.Ssc.GodTossing.Types.Base (CommitmentsMap, OpeningsMap, SharesMap,
                                                getOpening)
import           Pos.Types                     (Address (..), SharedSeed)
import           Pos.Util                      (getKeys)


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
calculateSeed (fromIntegral -> t) commitments openings lShares = do
    let participants = getKeys commitments

    -- First let's do some sanity checks.
    let extraOpenings, extraShares :: HashSet Address
        extraOpenings = HS.difference (getKeys openings) participants
        --We check that nodes which sent its encrypted shares to restore its opening
        --as well send their commitment. (e.g HM.member pkFrom participants)
        --We intentionally don't check, that nodes which decrypted shares
        --sent its commitments.
        --If node decrypted shares correctly, such node is useful for us, despite of
        --it didn't send its commitment.
        extraShares =
            let xs = mconcat (map getKeys (toList lShares))
            in  HS.difference xs participants
    unless (null extraOpenings) $
        Left (ExtraneousOpenings extraOpenings)
    unless (null extraShares) $
        Left (ExtraneousShares extraShares)

    -- And let's check openings.
    for_ (HM.toList commitments) $ \(key, view _2 -> commitment) -> do
        whenJust (HM.lookup key openings) $ \opening ->
            unless (verifyOpening commitment opening) $
                Left (BrokenCommitment key)

    -- Then we can start calculating seed, but first we have to recover some
    -- secrets (if corresponding openings weren't posted)

    -- Participants for whom we have to recover the secret
    let mustBeRecovered :: HashSet Address
        mustBeRecovered = HS.difference participants (getKeys openings)

    shares <- mapHelper BrokenShare (traverse deserializeM) lShares

    -- Secrets recovered from actual share lists (but only those we need –
    -- i.e. ones which are in mustBeRecovered)
    let recovered :: HashMap Address (Maybe Secret)
        recovered = HM.fromList $ do
            -- We are now trying to recover a secret for key 'k'
            k <- toList mustBeRecovered
            -- We collect all secrets that 'k' has sent to other nodes (and
            -- remove shares with equal IDs; when [CSL-206] is done, I assume
            -- we won't have to do this).
            --
            -- TODO: can we be sure that here different IDs mean different
            -- shares? maybe it'd be better to 'assert' it.
            let secrets :: [Share]
                secrets = toList . HM.fromList $
                          map (shareId &&& identity) $
                          mapMaybe (HM.lookup k) (toList shares)
            -- Then we recover the secret
            return (k, if length secrets < t
                         then Nothing
                         else Just $ unsafeRecoverSecret (take t secrets))

    secrets0 <- mapHelper BrokenSecret deserializeM $ getOpening <$> openings

    -- All secrets, both recovered and from openings
    let secrets :: HashMap Address Secret
        secrets = secrets0 <>
                  HM.mapMaybe identity recovered

    -- Now that we have the secrets, we can check whether the commitments
    -- actually match the secrets, and whether a secret has been recovered
    -- for each participant.
    --
    -- [CSL-50]
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

mapHelper :: (Address -> c) -> (b -> Maybe a) -> HashMap Address b -> Either c (HashMap Address a)
mapHelper errMapper mapper = HM.traverseWithKey (\pk v -> maybe (Left $ errMapper pk) Right $ mapper v)
