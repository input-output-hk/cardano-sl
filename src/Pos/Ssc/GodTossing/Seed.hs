-- | Actual shared seed calculation.

module Pos.Ssc.GodTossing.Seed
       ( calculateSeed
       ) where

import qualified Data.HashMap.Strict          as HM (fromList, lookup, map, mapMaybe,
                                                     mapWithKey, toList, traverseWithKey,
                                                     (!))
import qualified Data.HashSet                 as HS (difference)
import qualified Data.List.NonEmpty           as NE
import           Universum

import           Pos.Crypto                   (Secret, Share, unsafeRecoverSecret)
import           Pos.Ssc.GodTossing.Core      (Commitment (commShares),
                                               CommitmentsMap (getCommitmentsMap),
                                               GtDataId, MultiOpening (moOpenings),
                                               OpeningsMap, SharesMap, getOpening,
                                               secretToSharedSeed, toSignedCommitments,
                                               verifyOpening)
import           Pos.Ssc.GodTossing.Error     (SeedError (..))
import           Pos.Ssc.GodTossing.Functions (vssThreshold)
import           Pos.Types                    (SharedSeed)
import           Pos.Util                     (fromBinaryM, getKeys)


-- | Calculate SharedSeed. SharedSeed is a random bytestring that all
-- nodes generate together and agree on.
--
-- TODO: do we need to check secrets' lengths? Probably not.
calculateSeed ::
       CommitmentsMap        -- ^ All participating nodes
    -> OpeningsMap
    -> SharesMap             -- ^ Decrypted shares
    -> Either SeedError SharedSeed
calculateSeed (getCommitmentsMap -> multiCommitments) multiOpenings multiLShares = do
    --let toGtList ::
    let toGtData ex id md = toList $ NE.zipWith (\nm c -> ((id, nm), c)) (0 NE.:| [1..]) (ex md)
    --let toGtMap ::
    let toGtMap ex hm = HM.fromList $ concat $ toList $ HM.mapWithKey (toGtData ex) hm

    let commitments = toGtMap toSignedCommitments multiCommitments
    let openings = toGtMap moOpenings multiOpenings
    let lShares = HM.map (toGtMap identity) multiLShares
    let participants = getKeys commitments

    -- First let's do some sanity checks.
    let extraOpenings, extraShares :: HashSet GtDataId
        extraOpenings = (getKeys openings) `HS.difference` participants
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
    for_ (HM.toList commitments) $ \(id, (_, commitment, _)) -> do
        whenJust (HM.lookup id openings) $ \opening ->
            unless (verifyOpening commitment opening) $
                Left (BrokenCommitment id)

    -- Then we can start calculating seed, but first we have to recover some
    -- secrets (if corresponding openings weren't posted)

    -- Participants for whom we have to recover the secret
    let mustBeRecovered :: HashSet GtDataId
        mustBeRecovered = HS.difference participants (getKeys openings)

    shares <- mapHelper BrokenShare (traverse fromBinaryM) lShares

    -- Secrets recovered from actual share lists (but only those we need –
    -- i.e. ones which are in mustBeRecovered)
    let recovered :: HashMap GtDataId (Maybe Secret)
        recovered = HM.fromList $ do
            -- We are now trying to recover a secret for key 'k'
            id <- toList mustBeRecovered
            -- We collect all secrets that 'k' has sent to other nodes (and
            -- remove shares with equal IDs; when [CSL-206] is done, I assume
            -- we won't have to do this).
            --
            -- TODO: can we be sure that here different IDs mean different
            -- shares? maybe it'd be better to 'assert' it.
            let secrets :: [Share]
                secrets = mapMaybe (HM.lookup id) (toList shares)
            let t = fromIntegral . vssThreshold . length . commShares $
                        (commitments HM.! id) ^. _2
            -- Then we recover the secret
            return (id, if length secrets < t
                         then Nothing
                         else Just $ unsafeRecoverSecret (take t secrets))

    secrets0 <- mapHelper BrokenSecret fromBinaryM $ getOpening <$> openings

    -- All secrets, both recovered and from openings
    let secrets :: HashMap GtDataId Secret
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
       -- [CSL-481] We don't want to completely fail in case of SSC failures.
       --- | null secrets -> Left NoParticipants
       | otherwise    -> Right $
                         mconcat $ map secretToSharedSeed (toList secrets)

mapHelper
    :: (id -> c)
    -> (b -> Maybe a)
    -> HashMap id b
    -> Either c (HashMap id a)
mapHelper errMapper mapper =
    HM.traverseWithKey (\id v -> maybe (Left $ errMapper id) Right $ mapper v)
