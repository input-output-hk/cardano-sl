{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Major logic of Update System (US).

module Pos.Update.Logic
       ( usApplyBlocks
       , usRollbackBlocks
       , usVerifyBlocks
       ) where

import           Control.Lens         ((^.))
import           Control.Monad.Except (ExceptT, runExceptT, throwError)
import           Data.List            (partition)
import           Data.List.NonEmpty   (NonEmpty, groupWith)
import           Formatting           (sformat, (%))
import           Universum

import           Pos.Constants        (updateProposalThreshold, updateVoteThreshold)
import qualified Pos.DB               as DB
import           Pos.DB.Types         (ProposalState (..), mkProposalState,
                                       voteToProposalState)
import           Pos.Types            (Block, Coin, EpochIndex, NEBlocks, SlotId (..),
                                       UpdateProposal (..), UpdateVote (..), addressHash,
                                       applyCoinPortion, blockSlot, coinF, gbExtra,
                                       mebUpdate, mebUpdateVotes, mkCoin, prevBlockL,
                                       unsafeAddCoin)
import           Pos.Update.Error     (USError (..))
import           Pos.Util             (inAssertMode, maybeThrow, _neHead)
import           Pos.WorkMode         (WorkMode)

-- | Apply chain of /definitely/ valid blocks to US part of GState
-- DB and to US local data. Head must be the __oldest__ block.
usApplyBlocks :: WorkMode ssc m => NEBlocks ssc -> m [DB.SomeBatchOp]
usApplyBlocks blocks = do
    tip <- DB.getTip
    when (tip /= blocks ^. _neHead . prevBlockL) $ throwM $
        USCantApplyBlocks "oldest block in NEBlocks is not based on tip"
    inAssertMode $
        do verdict <- usVerifyBlocks blocks
           case verdict of
               Right _ -> pass
               Left errors ->
                   panic $ "usVerifyBlocks failed: " <> errors
    concat <$> mapM usApplyBlock (toList blocks)

usApplyBlock :: WorkMode ssc m => Block ssc -> m [DB.SomeBatchOp]
usApplyBlock (Left _) = pure []
-- Note: snapshot is not needed here, because we must have already
-- taken semaphore.
usApplyBlock (Right blk) = do
    let meb = blk ^. gbExtra
    let votes = meb ^. mebUpdateVotes
    let proposal = meb ^. mebUpdate
    let votePredicate vote =
            maybe False ((uvSoftware vote ==) . upSoftwareVersion) proposal
    let (curPropVotes, otherVotes) = partition votePredicate votes
    let otherGroups = groupWith uvSoftware otherVotes
    let slot = blk ^. blockSlot
    applyProposalBatch <- maybe (pure []) (applyProposal slot curPropVotes) proposal
    applyOtherVotesBatch <- concat <$> mapM applyVotesGroup otherGroups
    return (applyProposalBatch ++ applyOtherVotesBatch)

applyProposal
    :: WorkMode ssc m
    => SlotId -> [UpdateVote] -> UpdateProposal -> m [DB.SomeBatchOp]
applyProposal slot votes proposal =
    pure . DB.SomeBatchOp . DB.PutProposal <$>
    execStateT (mapM_ (applyVote epoch) votes) ps
  where
    ps = mkProposalState slot proposal
    epoch = siEpoch slot

-- Votes must be for the same update here.
applyVotesGroup
    :: WorkMode ssc m
    => NonEmpty UpdateVote -> m [DB.SomeBatchOp]
applyVotesGroup votes = do
    let sv = uvSoftware $ votes ^. _neHead
    ps <- maybeThrow (USUnknownSoftware sv) =<< DB.getProposalState sv
    let epoch = siEpoch $ psSlot ps
    pure . DB.SomeBatchOp . DB.PutProposal <$>
        execStateT (mapM_ (applyVote epoch) votes) ps

applyVote
    :: WorkMode ssc m
    => EpochIndex -> UpdateVote -> StateT ProposalState m ()
applyVote epoch UpdateVote {..} = do
    let id = addressHash uvKey
    stake <- maybeThrow (USNotRichmen id) =<< DB.getStakeUS epoch id
    modify $ voteToProposalState uvKey stake uvDecision

-- | Revert application of given blocks to US part of GState DB
-- and US local data. Head must be the __youngest__ block. Caller must
-- ensure that tip stored in DB is 'headerHash' of head.
--
-- FIXME: return Batch.
usRollbackBlocks :: WorkMode ssc m => NEBlocks ssc -> m ()
usRollbackBlocks _ = pass

-- | Verify whether sequence of blocks can be applied to US part of
-- current GState DB.  This function doesn't make pure checks,
-- they are assumed to be done earlier.
--
-- TODO: add more checks! Most likely it should be stateful!
usVerifyBlocks :: WorkMode ssc m => NEBlocks ssc -> m (Either Text ())
usVerifyBlocks = runExceptT . mapM_ verifyBlock

verifyBlock :: WorkMode ssc m => Block ssc -> ExceptT Text m ()
verifyBlock (Left _)    = pass
verifyBlock (Right blk) = do
    let meb = blk ^. gbExtra
    verifyEnoughStake (meb ^. mebUpdateVotes) (meb ^. mebUpdate)

verifyEnoughStake
    :: forall ssc m.
       WorkMode ssc m
    => [UpdateVote] -> Maybe UpdateProposal -> ExceptT Text m ()
verifyEnoughStake votes mProposal = do
    -- [CSL-314] Snapshot must be used here.
    totalStake <- maybe (pure zero) (const DB.getTotalFtsStake) mProposal
    let proposalThreshold = applyCoinPortion totalStake updateProposalThreshold
    let voteThreshold = applyCoinPortion totalStake updateVoteThreshold
    totalVotedStake <- verifyUpdProposalDo voteThreshold votes
    when (totalVotedStake < proposalThreshold) $
        throwError (msgProposal totalVotedStake proposalThreshold)
  where
    zero = mkCoin 0
    msgProposal =
        sformat
            ("update proposal doesn't have votes from enough stake ("
             %coinF%" < "%coinF%
             ")")
    msgVote =
        sformat
            ("update vote issuer doesn't have enough stake ("
             %coinF%" < "%coinF%
             ")")
    isVoteForProposal UpdateVote {..} =
        case mProposal of
            Nothing                    -> True
            Just (UpdateProposal {..}) -> uvDecision && uvSoftware == upSoftwareVersion
    verifyUpdProposalDo :: Coin -> [UpdateVote] -> ExceptT Text m Coin
    verifyUpdProposalDo _ [] = pure zero
    verifyUpdProposalDo voteThreshold (v@UpdateVote {..}:vs) = do
        let id = addressHash uvKey
        -- FIXME: use stake corresponding to state right before block
        -- corresponding to UpdateProposal for which vote is given is
        -- applied.
        stake <- fromMaybe zero <$> DB.getFtsStake id
        when (stake < voteThreshold) $ throwError $ msgVote stake voteThreshold
        let addedStake = if isVoteForProposal v then stake else zero
        unsafeAddCoin addedStake <$> verifyUpdProposalDo voteThreshold vs
