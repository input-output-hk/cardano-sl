-- | Actual shared seed calculation.

module Pos.Ssc.GodTossing.Seed
       ( calculateSeed
       ) where


import qualified Data.HashMap.Strict          as HM (fromList, lookup, mapMaybe,
                                                     traverseWithKey, (!))
import qualified Data.HashSet                 as HS (difference)
import           Universum

import           Pos.Crypto                   (Secret, Share, unsafeRecoverSecret)
import           Pos.Ssc.GodTossing.Core      (Commitment (commShares),
                                               CommitmentsMap (getCommitmentsMap),
                                               OpeningsMap, SharesMap, getOpening,
                                               secretToSharedSeed, verifyOpening)
import           Pos.Ssc.GodTossing.Error     (SeedError (..))
import           Pos.Ssc.GodTossing.Functions (vssThreshold)
import           Pos.Types                    (SharedSeed, StakeholderId, addressHash)
import           Pos.Util                     (fromBinaryM, getKeys)


-- | Calculate SharedSeed. SharedSeed is a random bytestring that all
-- nodes generate together and agree on.
--
-- TODO: do we need to check secrets' lengths? Probably not.
calculateSeed
    :: CommitmentsMap        -- ^ All participating nodes
    -> OpeningsMap
    -> SharesMap             -- ^ Decrypted shares
    -> Either SeedError SharedSeed
calculateSeed (getCommitmentsMap -> commitments) openings lShares = do
    let participants = getKeys commitments

    -- First let's do some sanity checks.
    let extraOpenings, extraShares :: HashSet StakeholderId
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
    for_ (toList commitments) $ \(pk, commitment, _) -> do
        let id = addressHash pk
        whenJust (HM.lookup id openings) $ \opening ->
            unless (verifyOpening commitment opening) $
                Left (BrokenCommitment id)

    -- Then we can start calculating seed, but first we have to recover some
    -- secrets (if corresponding openings weren't posted)

    -- Participants for whom we have to recover the secret
    let mustBeRecovered :: HashSet StakeholderId
        mustBeRecovered = HS.difference participants (getKeys openings)

    shares <- mapHelper BrokenShare (traverse fromBinaryM) lShares

    -- Secrets recovered from actual share lists (but only those we need –
    -- i.e. ones which are in mustBeRecovered)
    let recovered :: HashMap StakeholderId (Maybe Secret)
        recovered = HM.fromList $ do
            -- We are now trying to recover a secret for key 'k'
            id <- toList mustBeRecovered
            -- We collect all shares that 'k' has sent to other nodes
            let decryptedShares :: [Share]
                decryptedShares = mapMaybe (HM.lookup id) (toList shares)
            let threshold = fromIntegral . vssThreshold . length . commShares $
                            (commitments HM.! id) ^. _2
            -- Then we recover the secret.
            --   * All encrypted shares in commitments are valid, because we
            --     have checked them with 'verifyEncShare'. (I'm not sure how
            --     much 'verifyEncShare' actually checks, though.)
            --   * All decrypted shares match the shares in the commitments,
            --     because we have checked them with 'verifyShare'.
            --   * All shares have been encrypted for different nodes (and
            --     hence have different IDs), because if the block creator
            --     tries to lie that X's share sent to A was actually sent
            --     to B, we would notice that the share sent “to B” doesn't
            --     match the encrypted share in X's commitment.
            return (id, if length decryptedShares < threshold
                          then Nothing
                          else Just $ unsafeRecoverSecret
                                      (take threshold decryptedShares))

    secrets0 <- mapHelper BrokenSecret fromBinaryM $ getOpening <$> openings

    -- All secrets, both recovered and from openings
    let secrets :: HashMap StakeholderId Secret
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
    :: (StakeholderId -> c)
    -> (b -> Maybe a)
    -> HashMap StakeholderId b
    -> Either c (HashMap StakeholderId a)
mapHelper errMapper mapper = HM.traverseWithKey (\pk v -> maybe (Left $ errMapper pk) Right $ mapper v)
