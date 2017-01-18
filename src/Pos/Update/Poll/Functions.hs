{-# LANGUAGE BangPatterns #-}

-- | Functions which operate on MonadPoll[Read].

module Pos.Update.Poll.Functions
       ( verifyAndApplyUSPayload
       , rollbackUSPayload
       , normalizePoll
       ) where

import           Control.Monad.Except  (MonadError, throwError)
import           Data.List             (partition)
import           Data.List.NonEmpty    (NonEmpty)
import qualified Data.List.NonEmpty    as NE
import           Exceptions            (note)
import           Universum

import           Pos.Constants         (updateProposalThreshold, updateVoteThreshold)
import           Pos.Crypto            (hash)
import           Pos.Types             (ChainDifficulty, Coin, EpochIndex,
                                        SlotId (siEpoch), SoftwareVersion (..),
                                        addressHash, applyCoinPortion, coinToInteger,
                                        sumCoins, unsafeIntegerToCoin)
import           Pos.Update.Core       (UpId, UpdatePayload (..), UpdateProposal (..),
                                        UpdateVote (..))
import           Pos.Update.Poll.Class (MonadPoll (..), MonadPollRead (..))
import           Pos.Update.Poll.Types (PollVerFailure (..), ProposalState (..),
                                        USUndo (..), UndecidedProposalState (..))

----------------------------------------------------------------------------
-- Verify and apply
----------------------------------------------------------------------------

-- | Verify UpdatePayload with respect to data provided by
-- MonadPoll. If data is valid it is also applied.  Otherwise
-- PollVerificationFailure is thrown using MonadError type class.
-- When first flag is true and proposal is present,
-- 'updateProposalThreshold' is checked for it, otherwise it's not
-- checked.
verifyAndApplyUSPayload
    :: (MonadError PollVerFailure m, MonadPoll m)
    => Bool -> EpochIndex -> UpdatePayload -> m USUndo
verifyAndApplyUSPayload considerPropThreshold epoch UpdatePayload {..} = do
    let upId = hash <$> upProposal
    let votePredicate vote = maybe False (uvProposalId vote ==) upId
    let (curPropVotes, otherVotes) = partition votePredicate upVotes
    whenJust
        upProposal
        (verifyAndApplyProposal considerPropThreshold epoch curPropVotes)
    let otherGroups = NE.groupWith uvProposalId otherVotes
    mapM_ verifyAndApplyVotesGroup otherGroups
    return USUndo

resolveVoteStake
    :: (MonadError PollVerFailure m, MonadPollRead m)
    => EpochIndex -> Coin -> UpdateVote -> m Coin
resolveVoteStake epoch totalStake UpdateVote {..} = do
    let !id = addressHash uvKey
    stake <- note (mkNotRichman id Nothing) =<< getRichmanStake epoch id
    when (stake < threshold) $ throwError $ mkNotRichman id (Just stake)
    return stake
  where
    threshold = applyCoinPortion updateVoteThreshold totalStake
    mkNotRichman id stake =
        PollNotRichman
        {pnrStakeholder = id, pnrThreshold = threshold, pnrStake = stake}

verifyAndApplyProposal
    :: (MonadError PollVerFailure m, MonadPoll m)
    => Bool -> EpochIndex -> [UpdateVote] -> UpdateProposal -> m ()
verifyAndApplyProposal considerThreshold epoch votes up@UpdateProposal {..} = do
    let !upId = hash up
    whenM (hasActiveProposal (svAppName upSoftwareVersion)) $
        throwError $ Poll2ndActiveProposal upSoftwareVersion
    verifyAndApplyProposalScript upId up
    verifySoftwareVersion upId up
    totalStake <- note (PollUnknownStakes epoch) =<< getEpochTotalStake epoch
    votesAndStakes <-
        mapM (\v -> (v, ) <$> resolveVoteStake epoch totalStake v) votes
    when considerThreshold $ verifyProposalStake totalStake votesAndStakes upId

verifyAndApplyProposalScript
    :: (MonadError PollVerFailure m, MonadPoll m)
    => UpId -> UpdateProposal -> m ()
verifyAndApplyProposalScript upId UpdateProposal {..} =
    getScriptVersion upProtocolVersion >>= \case
        Nothing -> addScriptVersionDep upProtocolVersion upScriptVersion
        Just sv
            | sv == upScriptVersion -> pass
            | otherwise ->
                throwError
                    PollWrongScriptVersion
                    { pwsvExpected = sv
                    , pwsvFound = upScriptVersion
                    , pwsvUpId = upId
                    }

verifySoftwareVersion
    :: (MonadError PollVerFailure m, MonadPollRead m)
    => UpId -> UpdateProposal -> m ()
verifySoftwareVersion upId UpdateProposal {..} =
    getLastConfirmedSV app >>= \case
        Nothing -> pass
        Just n
            | svNumber sv == n -> pass
            | otherwise ->
                throwError
                    PollWrongSoftwareVersion
                    { pwsvStored = n
                    , pwsvGiven = svNumber sv
                    , pwsvApp = app
                    , pwsvUpId = upId
                    }
  where
    sv = upSoftwareVersion
    app = svAppName sv

verifyProposalStake
    :: (MonadError PollVerFailure m)
    => Coin -> [(UpdateVote, Coin)] -> UpId -> m ()
verifyProposalStake totalStake votesAndStakes upId = do
    let threshold = applyCoinPortion updateProposalThreshold totalStake
    let votesSum =
            sumCoins . map snd . filter (uvDecision . fst) $ votesAndStakes
    when (coinToInteger totalStake < votesSum) $
        throwError
            PollSmallProposalStake
            { pspsThreshold = threshold
            , pspsActual = unsafeIntegerToCoin votesSum
            , pspsUpId = upId
            }

-- Votes are assumed to be for the same proposal.
verifyAndApplyVotesGroup
    :: (MonadError PollVerFailure m, MonadPoll m)
    => NonEmpty UpdateVote -> m ()
verifyAndApplyVotesGroup votes = do
    let upId = uvProposalId $ NE.head votes
        !stakeholderId = addressHash . uvKey $ NE.head votes
        unknownProposalErr =
            PollUnknownProposal
            {pupStakeholder = stakeholderId, pupProposal = upId}
    ps <- note unknownProposalErr =<< getProposal upId
    case ps of
        PSDecided _     -> throwError $ PollProposalIsDecided upId stakeholderId
        PSUndecided ups -> mapM_ (verifyAndApplyVote ups) votes

verifyAndApplyVote
    :: (MonadError PollVerFailure m, MonadPoll m)
    => UndecidedProposalState -> UpdateVote -> m ()
verifyAndApplyVote UndecidedProposalState {..} v@UpdateVote {..} = do
    let e = siEpoch upsSlot
    totalStake <- note (PollUnknownStakes e) =<< getEpochTotalStake e
    _ <- resolveVoteStake e totalStake v
    pass

----------------------------------------------------------------------------
-- Rollback
----------------------------------------------------------------------------

-- | Rollback application of UpdatePayload in MonadPoll using payload
-- itself and undo data.
rollbackUSPayload
    :: MonadPoll m
    => ChainDifficulty -> UpdatePayload -> USUndo -> m ()
rollbackUSPayload _ _ _ = const pass notImplemented

----------------------------------------------------------------------------
-- Normalize
----------------------------------------------------------------------------

-- | Remove some data from Poll to make it valid. First argument
-- determines whether 'updateProposalThreshold' should be checked.
normalizePoll
    :: MonadPoll m
    => Bool -> m ()
normalizePoll _ = const pass notImplemented

----------------------------------------------------------------------------
-- Legacy garbage
----------------------------------------------------------------------------

-- usApplyBlock :: WorkMode ssc m => Block ssc -> m [DB.SomeBatchOp]
-- usApplyBlock (Left _) = pure []
-- -- Note: snapshot is not needed here, because we must have already
-- -- taken semaphore.
-- usApplyBlock (Right blk) = do
--     let UpdatePayload{..} = blk ^. gbBody.mbUpdatePayload
--     let upId = hash <$> upProposal
--     let votePredicate vote = maybe False (uvProposalId vote ==) upId
--     let (curPropVotes, otherVotes) = partition votePredicate upVotes
--     let slot = blk ^. blockSlot
--     applyProposalBatch   <- maybe (pure []) (applyProposal slot curPropVotes) upProposal
--     applyOtherVotesBatch <- concat <$> mapM applyVotesGroup otherGroups
--     return (applyProposalBatch ++ applyOtherVotesBatch)

-- applyProposal
--     :: WorkMode ssc m
--     => SlotId -> [UpdateVote] -> UpdateProposal -> m [DB.SomeBatchOp]
-- applyProposal slot votes proposal =
--     pure . DB.SomeBatchOp . GS.PutProposal . PSUndecided <$>
--     execStateT (mapM_ (applyVote epoch) votes) ps
--   where
--     ps = mkUProposalState slot proposal
--     epoch = siEpoch slot

-- -- Votes must be for the same update here.
-- applyVotesGroup
--     :: WorkMode ssc m
--     => NonEmpty UpdateVote -> m [DB.SomeBatchOp]
-- applyVotesGroup votes = do
--     let upId = uvProposalId $ votes ^. _neHead
--         -- TODO: here should be a procedure for getting a proposal state
--         -- from DB by UpId. Or what else should be here?
--         getProp = notImplemented
--     ps <- maybeThrow (USUnknownProposal upId) =<< getProp
--     case ps of
--         PSDecided _ -> pure []
--         PSUndecided ups -> do
--             let epoch = siEpoch $ upsSlot ups
--             pure . DB.SomeBatchOp . GS.PutProposal . PSUndecided <$>
--                 execStateT (mapM_ (applyVote epoch) votes) ups

-- applyVote
--     :: WorkMode ssc m
--     => EpochIndex -> UpdateVote -> StateT UndecidedProposalState m ()
-- applyVote epoch UpdateVote {..} = do
--     let id = addressHash uvKey
--     -- stake <- maybeThrow (USNotRichmen id) =<< GS.getStakeUS epoch id
--     stake <- maybeThrow (USNotRichmen id) =<< notImplemented epoch id
--     modify $ voteToUProposalState uvKey stake uvDecision

-- verifyEnoughStake
--     :: forall ssc m.
--        WorkMode ssc m
--     => [UpdateVote] -> Maybe UpdateProposal -> ExceptT Text m ()
-- verifyEnoughStake votes mProposal = do
--     -- [CSL-314] Snapshot must be used here.
--     totalStake <- maybe (pure zero) (const GS.getTotalFtsStake) mProposal
--     let proposalThreshold = applyCoinPortion updateProposalThreshold totalStake
--     let voteThreshold = applyCoinPortion updateVoteThreshold totalStake
--     totalVotedStake <- verifyUpdProposalDo voteThreshold votes
--     when (totalVotedStake < proposalThreshold) $
--         throwError (msgProposal totalVotedStake proposalThreshold)
--   where
--     zero = mkCoin 0
--     msgProposal =
--         sformat
--             ("update proposal doesn't have votes from enough stake ("
--              %coinF%" < "%coinF%
--              ")")
--     msgVote =
--         sformat
--             ("update vote issuer doesn't have enough stake ("
--              %coinF%" < "%coinF%
--              ")")
--     isVoteForProposal UpdateVote {..} =
--         case mProposal of
--             Nothing       -> True
--             Just proposal -> uvDecision && uvProposalId == hash proposal
--     verifyUpdProposalDo :: Coin -> [UpdateVote] -> ExceptT Text m Coin
--     verifyUpdProposalDo _ [] = pure zero
--     verifyUpdProposalDo voteThreshold (v@UpdateVote {..}:vs) = do
--         let id = addressHash uvKey
--         -- FIXME: use stake corresponding to state right before block
--         -- corresponding to UpdateProposal for which vote is given is
--         -- applied.
--         stake <- fromMaybe zero <$> GS.getFtsStake id
--         when (stake < voteThreshold) $ throwError $ msgVote stake voteThreshold
--         let addedStake = if isVoteForProposal v then stake else zero
--         unsafeAddCoin addedStake <$> verifyUpdProposalDo voteThreshold vs
