{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards     #-}

-- | Normalization logic in Poll.

module Pos.DB.Update.Poll.Logic.Normalize
       ( normalizePoll
       , refreshPoll
       , filterProposalsByThd
       ) where

import           Universum hiding (id)

import           Control.Lens (at, non)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Formatting (build, sformat, (%))

import           Pos.Chain.Update (BlockVersionData (..),
                     DecidedProposalState (..), LocalVotes, MonadPoll (..),
                     MonadPollRead (..), PollVerFailure (..),
                     ProposalState (..), UndecidedProposalState (..), UpId,
                     UpdateProposal, UpdateProposals, UpdateVote (..))
import           Pos.Core (Coin, EpochIndex, SlotId (siEpoch), addressHash,
                     applyCoinPortionUp, mkCoin, unsafeAddCoin)
import           Pos.Crypto (PublicKey, hash)
import           Pos.DB.Update.Poll.Logic.Apply (verifyAndApplyProposal,
                     verifyAndApplyVoteDo)
import           Pos.Util.Util (getKeys, sortWithMDesc)
import           Pos.Util.Wlog (logWarning)

-- | Normalize given proposals and votes with respect to current Poll
-- state, i. e. apply all valid data and discard invalid data.  This
-- function doesn't consider threshold which determines whether a
-- proposal can be put into a block.
normalizePoll
    :: (MonadPoll m)
    => BlockVersionData
    -> SlotId
    -> UpdateProposals
    -> LocalVotes
    -> m (UpdateProposals, LocalVotes)
normalizePoll genesisBvd slot proposals votes =
    (,) <$> normalizeProposals genesisBvd slot (toList proposals) <*>
    normalizeVotes genesisBvd (HM.toList votes)

-- | This function can be used to refresh mem pool consisting of given
-- proposals and votes. It applies the most valuable data and discards
-- everything else.
refreshPoll
    :: (MonadPoll m)
    => BlockVersionData
    -> SlotId
    -> UpdateProposals
    -> LocalVotes
    -> m (UpdateProposals, LocalVotes)
refreshPoll genesisBvd slot proposals votes = do
    proposalsSorted <- sortWithMDesc evaluatePropStake $ toList proposals
    -- When mempool is exhausted we leave only half of all proposals we have.
    -- We take proposals which have the greatest stake voted for it.
    let bestProposalsNum = length proposals `div` 2
    let bestProposals :: [UpdateProposal]
        bestProposals = take bestProposalsNum proposalsSorted
    -- We take all votes for proposals which we are going to leave in mempool.
    let votesForBest :: [(UpId, HashMap PublicKey UpdateVote)]
        votesForBest = mapMaybe propToVotes bestProposals
    let bestProposalsSet :: HashSet UpId
        bestProposalsSet = HS.fromList $ map hash bestProposals
    -- We also want to leave some votes for other proposals.
    let otherVotes :: [UpdateVote]
        otherVotes =
            concatMap (toList . snd) $
            filter (not . flip HS.member bestProposalsSet . fst) $
            HM.toList votes
    -- Again, we sort them by stake and take those having the greatest stake.
    otherVotesSorted <- sortWithMDesc evaluateVoteStake otherVotes
    let otherVotesNum = length otherVotes `div` 2
    let bestVotes =
            votesForBest <> groupVotes (take otherVotesNum otherVotesSorted)
    (,)
        <$> normalizeProposals genesisBvd slot bestProposals
        <*> normalizeVotes genesisBvd bestVotes
  where
    evaluatePropStake up =
        case votes ^. at (hash up) of
            Nothing         -> pure (mkCoin 0)
            Just votesForUP -> foldM step (mkCoin 0) (toList votesForUP)
    step accum vote
        | not (uvDecision vote) = pure accum
        | otherwise = unsafeAddCoin accum <$> evaluateVoteStake vote
    propToVotes up =
        let id = hash up
        in (id, ) <$> votes ^. at id
    evaluateVoteStake vote =
        fromMaybe (mkCoin 0) <$>
        getRichmanStake genesisBvd (siEpoch slot) (addressHash (uvKey vote))
    groupVotes :: [UpdateVote] -> [(UpId, HashMap PublicKey UpdateVote)]
    groupVotes = HM.toList . foldl' groupVotesStep mempty
    groupVotesStep :: LocalVotes -> UpdateVote -> LocalVotes
    groupVotesStep curVotes vote =
        curVotes & at (uvProposalId vote) . non mempty . at (uvKey vote)
                     .~ Just vote

-- Apply proposals which can be applied and put them in result.
-- Disregard other proposals.
normalizeProposals
    :: (MonadPoll m)
    => BlockVersionData -> SlotId -> [UpdateProposal] -> m UpdateProposals
normalizeProposals genesisBvd slotId (toList -> proposals) =
    HM.fromList . map ((\x->(hash x, x)) . fst) . catRights proposals <$>
    -- Here we don't need to verify that attributes are known, because it
    -- must hold for all proposals in mempool anyway.
    forM proposals
        (runExceptT . verifyAndApplyProposal genesisBvd False (Left slotId) [])

-- Apply votes which can be applied and put them in result.
-- Disregard other votes.
normalizeVotes
    :: forall m . (MonadPoll m)
    => BlockVersionData -> [(UpId, HashMap PublicKey UpdateVote)] -> m LocalVotes
normalizeVotes genesisBvd votesGroups =
    HM.fromList . catMaybes <$> mapM verifyNApplyVotesGroup votesGroups
  where
    verifyNApplyVotesGroup :: (UpId, HashMap PublicKey UpdateVote)
                           -> m (Maybe (UpId, HashMap PublicKey UpdateVote))
    verifyNApplyVotesGroup (upId, votesGroup) = getProposal upId >>= \case
        Nothing -> Nothing <$
                   logWarning (
                       sformat ("Update Proposal with id "%build%
                                " not found in normalizeVotes") upId)
        Just ps
            | PSUndecided ups <- ps -> do
                let pks = HM.keys votesGroup
                let uvs = toList votesGroup
                verifiedPKs <-
                  catRights pks <$>
                  mapM (runExceptT . verifyAndApplyVoteDo genesisBvd Nothing ups) uvs
                if | null verifiedPKs -> pure Nothing
                   | otherwise  ->
                       pure $ Just ( upId
                                   , votesGroup `HM.intersection`
                                     (HM.fromList verifiedPKs))
            | otherwise  -> pure Nothing

-- | Leave only those proposals which have enough stake for inclusion into
-- block according to 'bvdUpdateProposalThd'. Note that this function is
-- read-only.
filterProposalsByThd
    :: forall m
     . (MonadPollRead m)
    => BlockVersionData
    -> EpochIndex
    -> UpdateProposals
    -> m (UpdateProposals, HashSet UpId)
filterProposalsByThd genesisBvd epoch proposalsHM = getEpochTotalStake genesisBvd epoch >>= \case
    Nothing ->
        (mempty, getKeys proposalsHM) <$
            logWarning
                (sformat ("Couldn't get stake in filterProposalsByTxd for epoch "%build)
                         epoch)
    Just totalStake -> do
        thresholdPortion <- bvdUpdateProposalThd <$> getAdoptedBVData
        let threshold = applyCoinPortionUp thresholdPortion totalStake
        let proposals = HM.toList proposalsHM
        filtered <-
            HM.fromList <$> filterM (hasEnoughStake threshold . fst) proposals
        pure ( filtered
             , HS.fromList $ HM.keys $ proposalsHM `HM.difference` filtered)
  where
    hasEnoughStake :: Coin -> UpId -> m Bool
    hasEnoughStake threshold id = getProposal id >>= \case
        Nothing -> pure False
        Just (PSUndecided UndecidedProposalState {..} ) ->
            pure $ upsPositiveStake >= threshold
        Just (PSDecided DecidedProposalState {..} ) ->
            pure $ upsPositiveStake dpsUndecided >= threshold

catRights :: [a] -> [Either PollVerFailure b] -> [(a, b)]
catRights a b = catRightsDo $ zip a b
  where
    catRightsDo []                = []
    catRightsDo ((_, Left _):xs)  = catRightsDo xs
    catRightsDo ((x, Right y):xs) = (x, y):catRightsDo xs
