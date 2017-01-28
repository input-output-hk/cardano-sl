{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Normalization logic in Poll.

module Pos.Update.Poll.Logic.Normalize
       ( normalizePoll
       , filterProposalsByThd
       ) where

import qualified Data.HashMap.Strict         as HM
import qualified Data.HashSet                as HS
import           Formatting                  (build, sformat, (%))
import           System.Wlog                 (logWarning)
import           Universum

import           Pos.Constants               (genesisUpdateProposalThd)
import           Pos.Crypto                  (PublicKey, hash)
import           Pos.Ssc.Class               (Ssc)
import           Pos.Types                   (Coin, EpochIndex, SlotId, applyCoinPortion)
import           Pos.Update.Core             (LocalVotes, UpId, UpdateProposals,
                                              UpdateVote (..))
import           Pos.Update.Poll.Class       (MonadPoll (..), MonadPollRead (..))
import           Pos.Update.Poll.Logic.Apply (verifyAndApplyProposal,
                                              verifyAndApplyVoteDo)
import           Pos.Update.Poll.Types       (DecidedProposalState (..),
                                              PollVerFailure (..), ProposalState (..),
                                              UndecidedProposalState (..))
import           Pos.Util                    (getKeys)

-- | Normalize given proposals and votes with respect to current Poll
-- state, i. e. remove everything that is invalid. Valid data is
-- applied.  This function doesn't consider 'genesisUpdateProposalThd'.
normalizePoll
    :: forall ssc m . (MonadPoll m, Ssc ssc)
    => SlotId
    -> UpdateProposals
    -> LocalVotes
    -> m (UpdateProposals, LocalVotes)
normalizePoll slot proposals votes =
    (,) <$> normalizeProposals @ssc slot proposals <*> normalizeVotes votes

-- Apply proposals which can be applied and put them in result.
-- Disregard other proposals.
normalizeProposals
    :: forall ssc m . (MonadPoll m, Ssc ssc)
    => SlotId -> UpdateProposals -> m UpdateProposals
normalizeProposals slotId (toList -> proposals) =
    HM.fromList . map (\x->(hash x, x)) . map fst . catRights proposals <$>
    forM proposals
        (runExceptT . verifyAndApplyProposal @ssc False (Left slotId) [])

-- Apply votes which can be applied and put them in result.
-- Disregard other votes.
normalizeVotes
    :: forall m . (MonadPoll m)
    => LocalVotes -> m LocalVotes
normalizeVotes (HM.toList -> votesGroups) =
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
                  mapM (runExceptT . verifyAndApplyVoteDo Nothing ups) uvs
                if | null verifiedPKs -> pure Nothing
                   | otherwise  ->
                       pure $ Just ( upId
                                   , votesGroup `HM.intersection`
                                     (HM.fromList verifiedPKs))
            | otherwise  -> pure Nothing

-- Leave only those proposals which have enough stake for inclusion
-- into block according to 'genesisUpdateProposalThd'. Note that this
-- function is read-only.
filterProposalsByThd
    :: forall m . (MonadPollRead m)
    => EpochIndex -> UpdateProposals -> m (UpdateProposals, HashSet UpId)
filterProposalsByThd epoch proposalsHM = getEpochTotalStake epoch >>= \case
    Nothing ->
        (mempty, getKeys proposalsHM) <$
            logWarning
                (sformat ("Couldn't get stake in filterProposalsByTxd for epoch "%build)
                         epoch)
    Just totalStake -> do
        let threshold = applyCoinPortion genesisUpdateProposalThd totalStake
        let proposals = HM.toList proposalsHM
        filtered <-
            HM.fromList <$> filterM (hasEnoughtStake threshold . fst) proposals
        pure ( filtered
             , HS.fromList $ HM.keys $ proposalsHM `HM.difference` filtered)
  where
    hasEnoughtStake :: Coin -> UpId -> m Bool
    hasEnoughtStake threshold id = getProposal id >>= \case
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
