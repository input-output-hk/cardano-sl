-- | Major logic of Update System (US).

module Pos.Update.Logic
       ( usApplyBlocks
       , usRollbackBlocks
       , usVerifyBlocks
       ) where

import           Control.Lens         ((^.))
import           Formatting           (sformat, (%))
import           Serokell.Util.Verify (VerificationRes (..))
import           Universum

import           Pos.Constants        (updateProposalThreshold)
import qualified Pos.DB               as DB
import           Pos.Types            (Block, NEBlocks, UpdateProposal (..),
                                       UpdateVote (..), addressHash, applyCoinPortion,
                                       coinF, gbExtra, mebUpdate, mebUpdateVotes, mkCoin,
                                       unsafeSubCoin)
import           Pos.WorkMode         (WorkMode)

-- | Apply chain of /definitely/ valid blocks to US part of GState
-- DB and to US local data. Head must be the __oldest__ block.
--
-- FIXME: return Batch.
usApplyBlocks :: WorkMode ssc m => NEBlocks ssc -> m ()
usApplyBlocks _ = pass

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
usVerifyBlocks :: WorkMode ssc m => NEBlocks ssc -> m VerificationRes
usVerifyBlocks blks = fold <$> mapM verifyBlock blks

verifyBlock :: WorkMode ssc m => Block ssc -> m VerificationRes
verifyBlock (Left _)    = pure mempty
verifyBlock (Right blk) = do
    let meb = blk ^. gbExtra
    enoughStakeVerRes <-
        maybe
            (pure mempty)
            (verifyUpdProposal (meb ^. mebUpdateVotes))
            (meb ^. mebUpdate)
    return enoughStakeVerRes

verifyUpdProposal
    :: WorkMode ssc m
    => [UpdateVote] -> UpdateProposal -> m VerificationRes
verifyUpdProposal votes UpdateProposal {..} = do
    totalStake <- DB.getTotalFtsStake
    let threshold = applyCoinPortion totalStake updateProposalThreshold
    verifyUpdProposalDo votes threshold
  where
    msg =
        sformat
            ("update proposal doesn't have votes from enough stake, need " %coinF %
             " above available")
    verifyUpdProposalDo [] remainder = pure $ VerFailure [msg remainder]
    verifyUpdProposalDo (UpdateVote {..}:vs) remainder
        | uvDecision && uvSoftware == upSoftwareVersion = do
            let id = addressHash uvKey
            stake <- fromMaybe (mkCoin 0) <$> DB.getFtsStake id
            if stake < remainder
                then verifyUpdProposalDo vs (remainder `unsafeSubCoin` stake)
                else return VerSuccess
        | otherwise = verifyUpdProposalDo vs remainder
