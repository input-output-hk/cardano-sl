{-# LANGUAGE ScopedTypeVariables #-}

-- | Calculate shared seed using info sent by nodes participating in the
-- protocol.
module Pos.Ssc.Seed
       ( calculateSeed
       ) where

import           Universum hiding (id)

import           Control.Lens (_Left)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

import           Pos.Binary.Class (AsBinary, fromBinary)
import           Pos.Core (SharedSeed, StakeholderId, addressHash, mkCoin, sumCoins,
                           unsafeIntegerToCoin)
import           Pos.Core.Ssc (Commitment (..), CommitmentsMap (..), Opening (..), OpeningsMap,
                               SharesMap, SignedCommitment, getCommShares)
import           Pos.Crypto (DecShare, Secret, VssPublicKey, recoverSecret, verifySecret)
import           Pos.Lrc.Types (RichmenStakes)
import           Pos.Ssc.Base (secretToSharedSeed, verifyOpening, vssThreshold)
import           Pos.Ssc.Error.Seed (SscSeedError (..))
import           Pos.Util.Util (getKeys)

-- | Calculate SharedSeed. SharedSeed is a random bytestring that all
-- nodes generate together and agree on.
--
-- A story of why we pass nodes' VSS keys here: SCRAPE needs to know the
-- order of shares (i.e. you can't give it shares 4, 1 and 3, you must
-- necessarily give it 1,3,4). It also can't deduce the order of shares
-- because the shares don't carry any IDs or anything. Thus we need to
-- impose some arbitrary order on them. I chose to order the shares by
-- stakeholder's VSS key, which means that we need to know stakeholders' VSS
-- keys in addition to a `Stakeholder->[Share]` map if we want to recover
-- the secret.
calculateSeed
    :: CommitmentsMap                         -- ^ All participating nodes
    -> HashMap StakeholderId (AsBinary VssPublicKey) -- ^ Nodes' VSS keys
    -> OpeningsMap                            -- ^ Openings sent by the nodes
    -> SharesMap                              -- ^ Decrypted shares
    -> RichmenStakes                          -- ^ How much stake nodes have
    -> Either SscSeedError SharedSeed
calculateSeed commitments' binVssKeys openings lShares richmen = do
    let commitments :: HashMap StakeholderId SignedCommitment
        commitments = getCommitmentsMap commitments' -- just unwrapping
    let participants :: HashSet StakeholderId
        participants = getKeys commitments
    vssKeys :: HashMap StakeholderId VssPublicKey <-
        over _Left BrokenVssKey $ mapFailing (rightToMaybe . fromBinary) binVssKeys

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

    -- A 'SharesMap' with decoded shares:
    --
    -- shares :: HashMap StakeholderId
    --                   (HashMap StakeholderId (NonEmpty DecShare))
    shares <- over _Left BrokenShare $
              mapFailing (traverse (traverse (rightToMaybe . fromBinary))) lShares
    -- Get shares of some particular stakeholder's secret
    let getShares :: StakeholderId -> HashMap VssPublicKey (NonEmpty DecShare)
        getShares sender = HM.fromList $ catMaybes $ do
            (recipient, innerShareMap) <- HM.toList shares
            pure $ (,) <$> HM.lookup recipient vssKeys
                       <*> HM.lookup sender innerShareMap

    -- Secrets recovered from actual share lists (but only those we need –
    -- i.e. ones which are in mustBeRecovered)
    let recovered :: HashMap StakeholderId (Maybe Secret)
        recovered = HM.fromList $
            map (identity &&& recover) (toList mustBeRecovered)
        recover :: StakeholderId -> Maybe Secret
        recover key = do
            -- When we recover the secret, the following conditions are true:
            --   * All encrypted shares in commitments are valid, because we
            --     have checked them with 'verifyEncShares'.
            --   * All decrypted shares match the shares in the commitments,
            --     because we have checked them with 'verifyDecShare'.
            --
            -- Okay, here goes. Get the commitment:
            (_, comm@Commitment{..}, _) <- HM.lookup key commitments
            -- Calculate the threshold from amount of shares:
            let threshold = vssThreshold $ sum (HM.map length commShares)
            -- Get shareholders and their amounts of shares:
            shareholders <- map (over _2 length) <$> getCommShares comm
            -- Recover the secret
            secret <- recoverSecret
                        threshold
                        shareholders
                        (fmap toList (getShares key))
            -- Verify the recovered secret against the commitment
            guard (verifySecret threshold commProof secret)
            pure secret

    secrets0 <- over _Left BrokenSecret $
                mapFailing (rightToMaybe . fromBinary . getOpening) openings

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
