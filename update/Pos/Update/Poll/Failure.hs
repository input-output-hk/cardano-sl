-- | Failures which can happen in Poll.

module Pos.Update.Poll.Failure
       ( PollVerFailure (..)
       , reportUnexpectedError
       ) where

import           Universum

import qualified Data.Text.Buildable
import           Formatting (bprint, build, int, sformat, stext, (%))
import           Serokell.Data.Memory.Units (Byte, memory)

import           Pos.Core (ApplicationName, BlockVersion, BlockVersionData, Coin, EpochIndex,
                           HeaderHash, NumSoftwareVersion, ScriptVersion, StakeholderId, coinF)
import           Pos.Core.Update (BlockVersionModifier, UpAttributes, UpId)
import           Pos.Crypto (shortHashF)
import           Pos.Reporting (MonadReporting, reportError)

-- | PollVerFailure represents all possible errors which can
-- appear in Poll data verification.
data PollVerFailure
    =
      -- | 'BlockVersionModifier' for this 'BlockVersion' is already known and
      -- the one we saw doesn't match it.
      PollInconsistentBVM { pibExpected :: !BlockVersionModifier
                          , pibFound    :: !BlockVersionModifier
                          , pibUpId     :: !UpId}
    -- | 'BlockVersion' is already adopted and 'BlockVersionData' associated
    -- with it differs from the one we saw.
    | PollAlreadyAdoptedDiffers { paadAdopted  :: !BlockVersionData
                                , paadProposed :: !BlockVersionModifier
                                , paadUpId     :: !UpId}
    -- | Proposed script version must be the same as adopted one or
    -- greater by one, but this rule is violated.
    | PollWrongScriptVersion { pwsvAdopted  :: !ScriptVersion
                             , pwsvProposed :: !ScriptVersion
                             , pwsvUpId     :: !UpId}
    -- | A proposal tried to increase the block size limit more than it was
    -- allowed to
    | PollLargeMaxBlockSize { plmbsMaxPossible :: !Byte
                            , plmbsFound       :: !Byte
                            , plmbsUpId        :: !UpId}
    -- | A proposal attempted to change the end of the bootstrap era
    -- post factum
    | PollBootstrapEraInvalidChange { pbeicLast     :: !EpochIndex
                                    , pbeicAdopted  :: !EpochIndex
                                    , pbeicProposed :: !EpochIndex
                                    , pbeicUpId     :: !UpId }
    | PollNotFoundScriptVersion !BlockVersion
    | PollProposalAlreadyActive !UpId
    | PollSmallProposalStake { pspsThreshold :: !Coin
                            ,  pspsActual    :: !Coin
                            ,  pspsUpId      :: !UpId}
    | PollNotRichman { pnrStakeholder :: !StakeholderId
                    ,  pnrThreshold   :: !Coin
                    ,  pnrStake       :: !(Maybe Coin)}
    | PollUnknownProposal { pupStakeholder :: !StakeholderId
                         ,  pupProposal    :: !UpId}
    | PollUnknownStakes !EpochIndex
    | PollWrongSoftwareVersion { pwsvStored :: !(Maybe NumSoftwareVersion)
                              ,  pwsvApp    :: !ApplicationName
                              ,  pwsvGiven  :: !NumSoftwareVersion
                              ,  pwsvUpId   :: !UpId}
    | PollProposalIsDecided { ppidUpId        :: !UpId
                           ,  ppidStakeholder :: !StakeholderId}
    | PollExtraRevote { perUpId        :: !UpId
                     ,  perStakeholder :: !StakeholderId
                     ,  perDecision    :: !Bool}
    | PollWrongHeaderBlockVersion { pwhpvGiven   :: !BlockVersion
                                  , pwhpvAdopted :: !BlockVersion}
    | PollBadBlockVersion { pbpvUpId       :: !UpId
                            ,  pbpvGiven   :: !BlockVersion
                            ,  pbpvAdopted :: !BlockVersion}
    | PollTooLargeProposal { ptlpUpId  :: !UpId
                           , ptlpSize  :: !Byte
                           , ptlpLimit :: !Byte
                           }
    | PollMoreThanOneProposalPerEpoch { ptopFrom :: !StakeholderId
                                      , ptopUpId :: !UpId
                                      }
    | PollUnknownAttributesInProposal { puapUpId  :: !UpId
                                      , puapAttrs :: !UpAttributes
                                      }
    | PollTipMismatch { ptmTipDB     :: !HeaderHash
                      , ptmTipMemory :: !HeaderHash
                      }
    | PollInvalidUpdatePayload !Text
    | PollInternalError !Text

instance Buildable PollVerFailure where
    build (PollInconsistentBVM {..}) =
        bprint ("proposal "%shortHashF%" contains block version"%
                " which is already competing and its"%
                " BlockVersionModifier is different"%
                " (expected "%build%", proposed "%build%")")
        pibUpId pibExpected pibFound
    build (PollAlreadyAdoptedDiffers {..}) =
        bprint ("proposal "%shortHashF%" contains block version"%
                " which is already adopted and its"%
                " BlockVersionModifier doesn't correspond to the adopted"%
                " BlockVersionData (adopted "%build%", proposed "%build%")")
        paadUpId paadAdopted paadProposed
    build (PollWrongScriptVersion {..}) =
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
    build (PollWrongSoftwareVersion {..}) =
        bprint ("proposal "%build%" has wrong software version for app "%
                build%" (last known is "%stext%", proposal contains "%int%")")
        pwsvUpId pwsvApp (maybe "unknown" pretty pwsvStored) pwsvGiven
    build (PollProposalIsDecided {..}) =
        bprint ("proposal "%build%" is in decided state, but stakeholder "%
                build%" has voted for it")
        ppidUpId ppidStakeholder
    build (PollExtraRevote {..}) =
        bprint ("stakeholder "%build%" vote "%stext%" proposal "
                %build%" more than once")
        perStakeholder (bool "against" "for" perDecision) perUpId
    build (PollWrongHeaderBlockVersion {..}) =
        bprint ("wrong protocol version has been seen in header: "%
                build%" (current adopted is "%build%"), "%
                "this version is smaller than last adopted "%
                "or is not confirmed")
        pwhpvGiven pwhpvAdopted
    build (PollBadBlockVersion {..}) =
        bprint ("proposal "%build%" has bad protocol version: "%
                build%" (current adopted is "%build%")")
        pbpvUpId pbpvGiven pbpvAdopted
    build (PollTooLargeProposal {..}) =
        bprint ("update proposal "%shortHashF%" exceeds maximal size ("%
                int%" > "%int%")")
        ptlpUpId ptlpSize ptlpLimit
    build (PollMoreThanOneProposalPerEpoch {..}) =
        bprint ("stakeholder "%shortHashF%
                " proposed second proposal "%shortHashF%" in epoch")
        ptopFrom ptopUpId
    build (PollUnknownAttributesInProposal {..}) =
        bprint ("proposal "%shortHashF%" has unknown attributes "%build)
        puapUpId puapAttrs
    build (PollTipMismatch {..}) =
        bprint ("tip we store in US mem-state ("%shortHashF%
                ") differs from the tip we store in DB ("%build%")")
        ptmTipMemory ptmTipDB
    build (PollInvalidUpdatePayload msg) =
        bprint ("invalid update payload: "%stext) msg
    build (PollInternalError msg) =
        bprint ("internal error: "%stext) msg

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
