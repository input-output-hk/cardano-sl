-- | Failures which can happen in Poll.

module Pos.Chain.Update.Poll.Failure
       ( PollVerFailure (..)
       , reportUnexpectedError
       ) where

import           Universum hiding (id, last)

import           Formatting (bprint, build, int, sformat, stext, (%))
import qualified Formatting.Buildable
import           Serokell.Data.Memory.Units (Byte, memory)

import           Pos.Chain.Block.Header (HeaderHash)
import           Pos.Chain.Update.ApplicationName (ApplicationName)
import           Pos.Chain.Update.BlockVersion (BlockVersion)
import           Pos.Chain.Update.BlockVersionData (BlockVersionData)
import           Pos.Chain.Update.BlockVersionModifier (BlockVersionModifier)
import           Pos.Chain.Update.SoftwareVersion (NumSoftwareVersion)
import           Pos.Chain.Update.Vote (UpAttributes, UpId)
import           Pos.Core (Coin, EpochIndex, ScriptVersion, StakeholderId,
                     coinF)
import           Pos.Core.Reporting (MonadReporting, reportError)
import           Pos.Crypto (shortHashF)

-- | PollVerFailure represents all possible errors which can
-- appear in Poll data verification.
data PollVerFailure
    =
    -- | 'BlockVersionModifier' for this 'BlockVersion' is already known and
    -- the one we saw doesn't match it.
    -- PollInconsistentBVM
    --    pibExpected
    --    pibFound
    --    pibUpId
      PollInconsistentBVM !BlockVersionModifier !BlockVersionModifier !UpId
    -- | 'BlockVersion' is already adopted and 'BlockVersionData' associated
    -- with it differs from the one we saw.
    -- PollAlreadyAdoptedDiffers
    --     paadAdopted
    --     paadProposed
    --     paadUpId
    | PollAlreadyAdoptedDiffers !BlockVersionData !BlockVersionModifier !UpId
    -- | Proposed script version must be the same as adopted one or
    -- greater by one, but this rule is violated.
    -- PollWrongScriptVersion
    --    pwsvAdopted
    --    pwsvProposed
    --    pwsvUpId
    | PollWrongScriptVersion !ScriptVersion !ScriptVersion !UpId
    -- | A proposal tried to increase the block size limit more than it was
    -- allowed to
    -- PollLargeMaxBlockSize
    --    plmbsMaxPossible
    --    plmbsFound
    --    plmbsUpId
    | PollLargeMaxBlockSize !Byte !Byte !UpId
    -- | A proposal attempted to change the end of the bootstrap era
    -- post factum
    -- PollBootstrapEraInvalidChange
    --     pbeicLast
    --     pbeicAdopted
    --     pbeicProposed
    --     pbeicUpId
    | PollBootstrapEraInvalidChange !EpochIndex !EpochIndex !EpochIndex !UpId
    | PollNotFoundScriptVersion !BlockVersion
    | PollProposalAlreadyActive !UpId
    -- | PollSmallProposalStake
    --       pspsThreshold
    --       pspsActual
    --       pspsUpId
    | PollSmallProposalStake !Coin !Coin !UpId
    -- | PollNotRichman
    --       pnrStakeholder
    --       pnrThreshold
    --       pnrStake
    | PollNotRichman !StakeholderId !Coin !(Maybe Coin)
    -- | PollUnknownProposal
    --       pupStakeholder
    --       pupProposal
    | PollUnknownProposal !StakeholderId !UpId
    | PollUnknownStakes !EpochIndex
    -- PollWrongSoftwareVersion
    --    pwsvStored
    --    pwsvApp
    --    pwsvGiven
    --    pwsvUpId
    | PollWrongSoftwareVersion
          !(Maybe NumSoftwareVersion)
          !ApplicationName
          !NumSoftwareVersion
          !UpId
    -- | PollProposalIsDecided
    --      ppidUpId
    --      ppidStakeholder
    | PollProposalIsDecided !UpId !StakeholderId
    -- | PollExtraRevote
    --       perUpId
    --       perStakeholder
    --       perDecision
    | PollExtraRevote !UpId !StakeholderId !Bool
    -- | PollWrongHeaderBlockVersion
    --       pwhpvGiven
    --       pwhpvAdopted
    | PollWrongHeaderBlockVersion !BlockVersion !BlockVersion
    -- | PollBadBlockVersion
    --       pbpvUpId
    --       pbpvGiven
    --       pbpvAdopted
    | PollBadBlockVersion !UpId !BlockVersion !BlockVersion
    -- | PollTooLargeProposal
    --      ptlpUpId
    --      ptlpSize
    --      ptlpLimit
    | PollTooLargeProposal !UpId !Byte !Byte
    -- | PollMoreThanOneProposalPerEpoch
    --      ptopFrom
    --      ptopUpId
    | PollMoreThanOneProposalPerEpoch !StakeholderId !UpId
    -- | PollUnknownAttributesInProposal
    --       puapUpId
    --       puapAttrs
    | PollUnknownAttributesInProposal !UpId !UpAttributes
    -- | PollTipMismatch
    --       ptmTipDB
    --       ptmTipMemory
    | PollTipMismatch !HeaderHash !HeaderHash
    | PollInvalidUpdatePayload !Text
    | PollInternalError !Text
    | PollUpdateVersionNoChange !BlockVersion !NumSoftwareVersion

instance Buildable PollVerFailure where
    build (PollInconsistentBVM pibExpected pibFound pibUpId) =
        bprint ("proposal "%shortHashF%" contains block version"%
                " which is already competing and its"%
                " BlockVersionModifier is different"%
                " (expected "%build%", proposed "%build%")")
        pibUpId pibExpected pibFound
    build (PollAlreadyAdoptedDiffers paadAdopted paadProposed paadUpId) =
        bprint ("proposal "%shortHashF%" contains block version"%
                " which is already adopted and its"%
                " BlockVersionModifier doesn't correspond to the adopted"%
                " BlockVersionData (adopted "%build%", proposed "%build%")")
        paadUpId paadAdopted paadProposed
    build (PollWrongScriptVersion pwsvAdopted pwsvProposed pwsvUpId) =
        bprint ("proposal "%shortHashF%" contains script version"%
                " which is neither same not greater by one than the"%
                " adopted one (adopted one is "%int%
                ", proposed one is "%int%")")
        pwsvUpId pwsvAdopted pwsvProposed
    build (PollLargeMaxBlockSize maxPossible found upId) =
        bprint ("proposal "%build%" tried to increase max block size"%
                " beyond what is allowed"%
                " (expected max. "%memory%", found "%memory%")")
        upId maxPossible found
    build (PollBootstrapEraInvalidChange last adopted proposed upId) =
        bprint ("proposal "%build%" tried to change the end of the bootstrap"%
                " era to epoch"%build%", but the bootstrap era has ended with"%
                " unlock stakes epoch "%build%", and now the epoch is "%
                build%".")
        upId proposed adopted last
    build (PollProposalAlreadyActive upId) =
        bprint ("proposal "%build%" was already proposed") upId
    build (PollNotFoundScriptVersion pv) =
        bprint ("not found script version for protocol version "%build) pv
    build (PollSmallProposalStake threshold actual upId) =
        bprint ("proposal "%build%
                " doesn't have enough stake from positive votes "%
                "(threshold is "%coinF%", proposal has "%coinF%")")
        upId threshold actual
    build (PollNotRichman id threshold stake) =
        bprint ("voter "%build%" is not richman (his stake is "%stext%", but"%
                " threshold is "%coinF%")")
        id (maybe "negligible" (sformat coinF) stake) threshold
    build (PollUnknownProposal stakeholder proposal) =
        bprint (build%" has voted for unkown proposal "%build)
        stakeholder proposal
    build (PollUnknownStakes epoch) =
        bprint ("stake distribution for epoch "%build%" is unknown") epoch
    build (PollWrongSoftwareVersion pwsvStored pwsvApp pwsvGiven pwsvUpId) =
        bprint ("proposal "%build%" has wrong software version for app "%
                build%" (last known is "%stext%", proposal contains "%int%")")
        pwsvUpId pwsvApp (maybe "unknown" pretty pwsvStored) pwsvGiven
    build (PollProposalIsDecided
               ppidUpId
               ppidStakeholder) =
        bprint ("proposal "%build%" is in decided state, but stakeholder "%
                build%" has voted for it")
        ppidUpId ppidStakeholder
    build (PollExtraRevote perUpId perStakeholder perDecision) =
        bprint ("stakeholder "%build%" vote "%stext%" proposal "
                %build%" more than once")
        perStakeholder (bool "against" "for" perDecision) perUpId
    build (PollWrongHeaderBlockVersion pwhpvGiven pwhpvAdopted) =
        bprint ("wrong protocol version has been seen in header: "%
                build%" (current adopted is "%build%"), "%
                "this version is smaller than last adopted "%
                "or is not confirmed")
        pwhpvGiven pwhpvAdopted
    build (PollBadBlockVersion pbpvUpId pbpvGiven pbpvAdopted) =
        bprint ("proposal "%build%" has bad protocol version: "%
                build%" (current adopted is "%build%")")
        pbpvUpId pbpvGiven pbpvAdopted
    build (PollTooLargeProposal ptlpUpId ptlpSize ptlpLimit) =
        bprint ("update proposal "%shortHashF%" exceeds maximal size ("%
                int%" > "%int%")")
        ptlpUpId ptlpSize ptlpLimit
    build (PollMoreThanOneProposalPerEpoch ptopFrom ptopUpId) =
        bprint ("stakeholder "%shortHashF%
                " proposed second proposal "%shortHashF%" in epoch")
        ptopFrom ptopUpId
    build (PollUnknownAttributesInProposal puapUpId puapAttrs) =
        bprint ("proposal "%shortHashF%" has unknown attributes "%build)
        puapUpId puapAttrs
    build (PollTipMismatch ptmTipMemory ptmTipDB) =
        bprint ("tip we store in US mem-state ("%shortHashF%
                ") differs from the tip we store in DB ("%build%")")
        ptmTipMemory ptmTipDB
    build (PollInvalidUpdatePayload msg) =
        bprint ("invalid update payload: "%stext) msg
    build (PollInternalError msg) =
        bprint ("internal error: "%stext) msg
    build (PollUpdateVersionNoChange blockVer softVer) =
        bprint ("update did not increment the block version ("%build
               %") or the software version ("%build%").")
               blockVer softVer

-- | Report an error if it's unexpected.
--
-- If tips are different, we report error, because it's suspicious and
-- we want to review logs. If it's internal error, we definitely want
-- to investigate it.
reportUnexpectedError
    :: ( Monad m, MonadReporting m )
    => m (Either PollVerFailure a)
    -> m (Either PollVerFailure a)
reportUnexpectedError action = do
    res <- action
    -- REPORT:ERROR Internal error in update system or tips mismatch.
    res <$
        case res of
            Left (PollInternalError msg) ->
                reportError $
                "Internal error occurred in update system: " <> msg
            Left (err@(PollTipMismatch {})) -> reportError (pretty err)
            _ -> pass
