-- | Calculate shared seed using info sent by nodes participating in the
-- protocol.
module Pos.Ssc.GodTossing.Seed
       ( calculateSeed
       ) where

import           Universum

import           Control.Lens                 (_Left)
import qualified Data.HashMap.Strict          as HM
import qualified Data.HashSet                 as HS

import           Pos.Binary.Class             (fromBinary, fromBinaryM)
import           Pos.Core                     (SharedSeed, StakeholderId, addressHash,
                                               mkCoin, sumCoins, unsafeIntegerToCoin)
import           Pos.Crypto                   (Secret, Share, unsafeRecoverSecret,
                                               verifySecretProof)
import           Pos.Lrc.Types                (RichmenStake)
import           Pos.Ssc.GodTossing.Core      (Commitment (..),
                                               CommitmentsMap (getCommitmentsMap),
                                               OpeningsMap, SharesMap, getOpening,
                                               secretToSharedSeed, verifyOpening)
import           Pos.Ssc.GodTossing.Error     (SeedError (..))
import           Pos.Ssc.GodTossing.Functions (vssThreshold)
import           Pos.Util.Util                (getKeys)


-- | Calculate SharedSeed. SharedSeed is a random bytestring that all
-- nodes generate together and agree on.
--
-- TODO: do we need to check secrets' lengths? Probably not.
calculateSeed
    :: CommitmentsMap        -- ^ All participating nodes
    -> OpeningsMap           -- ^ Openings sent by those nodes
    -> SharesMap             -- ^ Decrypted shares
    -> RichmenStake          -- ^ How much stake nodes have
    -> Either SeedError SharedSeed
calculateSeed commitments' openings lShares richmen = do
    let commitments = getCommitmentsMap commitments' -- just unwrapping
    let participants = getKeys commitments

    -- First let's do some sanity checks.
    let nonRichmen :: HashSet StakeholderId
        nonRichmen = HS.difference participants (getKeys richmen)
    unless (null nonRichmen) $
        Left (NonRichmenParticipants nonRichmen)

    let extraOpenings, extraShares :: HashSet StakeholderId
        extraOpenings = HS.difference (getKeys openings) participants
        -- We check that nodes which sent its encrypted shares to restore its
        -- opening as well send their commitment. (e.g HM.member pkFrom
        -- participants) We intentionally don't check, that nodes which
        -- decrypted shares sent its commitments. If node has decrypted
        -- shares correctly, this node is useful for us, even if it didn't
        -- send its commitment.
        extraShares =
            let xs = mconcat (map getKeys (toList lShares))
            in  HS.difference xs participants
    unless (null extraOpenings) $
        Left (ExtraOpenings extraOpenings)
    unless (null extraShares) $
        Left (ExtraShares extraShares)

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

    shares <- over _Left BrokenShare $
              mapFailing (traverse (traverse fromBinaryM)) lShares

    -- Secrets recovered from actual share lists (but only those we need –
    -- i.e. ones which are in mustBeRecovered)
    let recovered :: HashMap StakeholderId (Maybe Secret)
        -- NB. magical mystery list monad is used here
        recovered = HM.fromList $ do
            -- We are now trying to recover a secret for key 'k'
            id <- toList mustBeRecovered
            -- We collect all shares that 'k' has sent to other nodes
            let decryptedShares :: [Share]
                decryptedShares = concatMap toList $
                                  mapMaybe (HM.lookup id) (toList shares)
            let t = fromIntegral . vssThreshold . sum .
                    HM.map length . commShares $ (commitments HM.! id) ^. _2
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
            let mbSecret = do
                    -- We must have enough shares to even try recovering
                    guard (length decryptedShares >= t)
                    let secret = unsafeRecoverSecret (take t decryptedShares)
                    -- Recovered secret must match the commitment
                    (_, Commitment{..}, _) <- HM.lookup id commitments
                    commExtra' <- rightToMaybe $ fromBinary commExtra
                    commProof' <- rightToMaybe $ fromBinary commProof
                    guard (verifySecretProof commExtra' secret commProof')
                    -- All checks passed
                    return secret
            return (id, mbSecret)

    secrets0 <- over _Left BrokenSecret $
                mapFailing (fromBinaryM . getOpening) openings

    -- All secrets, both recovered and from openings
    let secrets :: HashMap StakeholderId Secret
        secrets = secrets0 <> HM.mapMaybe identity recovered

    -- If only half of stake (or less) has participated in seed choice, we
    -- consider this MPC round a failure because it breaks security
    -- guarantees of the protocol.
    let getStake id = HM.lookupDefault (mkCoin 0) id richmen
        totalStake   = sumCoins (fmap getStake (HM.keys richmen))
        secretsStake = sumCoins (fmap getStake (HM.keys secrets))
    when (null secrets) $
        Left NoSecrets
    unless (secretsStake * 2 > totalStake) $
        Left $ NotEnoughParticipatingStake
                   -- these coins come from inside another part of CSL so
                   -- presumably they're safe to add
                   (unsafeIntegerToCoin secretsStake)
                   (unsafeIntegerToCoin totalStake)

    -- Now we just XOR all secrets together to obtain the shared seed
    return $ mconcat $ map secretToSharedSeed (toList secrets)

-- | Apply a (possibly failing) function to all elements of a map. If the
-- function fails, say what element it failed at.
mapFailing :: (a -> Maybe b) -> HashMap k a -> Either k (HashMap k b)
mapFailing f =
    HM.traverseWithKey (\k a -> maybe (Left k) Right $ f a)
