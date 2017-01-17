-- | Functions which operate on MonadPoll[Read].

module Pos.Update.Poll.Functions
       ( verifyAndApplyUSPayload
       , rollbackUSPayload
       , normalizePoll
       ) where

import           Control.Monad.Except  (MonadError)
import           Universum

import           Pos.Types.Types       (ChainDifficulty)
import           Pos.Update.Core       (UpdatePayload)
import           Pos.Update.Poll.Class (MonadPoll)
import           Pos.Update.Poll.Types (PollVerFailure, USUndo)

-- | Verify UpdatePayload with respect to data provided by
-- MonadPoll. If data is valid it is also applied.  Otherwise
-- PollVerificationFailure is thrown using MonadError type class.
-- When first flag is true and proposal is present,
-- 'updateProposalThreshold' is checked for it, otherwise it's not
-- checked.
verifyAndApplyUSPayload
    :: (MonadError PollVerFailure m, MonadPoll m)
    => Bool -> UpdatePayload -> m USUndo
verifyAndApplyUSPayload = notImplemented

-- | Rollback application of UpdatePayload in MonadPoll using payload
-- itself and undo data.
rollbackUSPayload
    :: MonadPoll m
    => ChainDifficulty -> UpdatePayload -> USUndo -> m ()
rollbackUSPayload = notImplemented

-- | Remove some data from Poll to make it valid. First argument
-- determines whether 'updateProposalThreshold' should be checked.
normalizePoll
    :: MonadPoll m
    => Bool -> m ()
normalizePoll = notImplemented

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
--     let otherGroups = groupWith uvProposalId otherVotes
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
