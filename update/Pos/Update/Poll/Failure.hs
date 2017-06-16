-- | Failures which can happen in Poll.

module Pos.Update.Poll.Failure
       ( PollVerFailure (..)
       ) where

import qualified Data.Text.Buildable
import           Data.Time.Units            (Millisecond)
import           Formatting                 (bprint, build, int, sformat, stext, (%))
import           Serokell.Data.Memory.Units (Byte)
import           Universum

import           Pos.Core.Coin              (coinF)
import           Pos.Core.Types             (ApplicationName, BlockVersion, Coin,
                                             EpochIndex, NumSoftwareVersion,
                                             ScriptVersion, StakeholderId)
import           Pos.Crypto                 (shortHashF)
import           Pos.Update.Core            (UpAttributes, UpId)

-- | PollVerFailure represents all possible errors which can
-- appear in Poll data verification.
data PollVerFailure
    = PollWrongScriptVersion { pwsvExpected :: !ScriptVersion
                             , pwsvFound    :: !ScriptVersion
                             , pwsvUpId     :: !UpId}
    -- | Slot duration for this block version is already known and the one we
    -- saw doesn't match it
    | PollWrongSlotDuration { pwsdExpected :: !Millisecond
                            , pwsdFound    :: !Millisecond
                            , pwsdUpId     :: !UpId}
    -- | Max block size for this block version is already known and the one
    -- we saw doesn't match it
    | PollWrongMaxBlockSize { pwmbsExpected :: !Byte
                            , pwmbsFound    :: !Byte
                            , pwmbsUpId     :: !UpId}
    -- | A proposal tried to increase the block size limit more than it was
    -- allowed to
    | PollLargeMaxBlockSize { plmbsMaxPossible :: !Byte
                            , plmbsFound       :: !Byte
                            , plmbsUpId        :: !UpId}
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
    | PollInternalError !Text

instance Buildable PollVerFailure where
    build (PollWrongScriptVersion expected found upId) =
        bprint ("wrong script version in proposal "%build%
                " (expected "%int%", found "%int%")")
        upId expected found
    build (PollWrongSlotDuration expected found upId) =
        bprint ("wrong slot duration in proposal "%build%
                " (expected "%int%", found "%int%")")
        upId expected found
    build (PollWrongMaxBlockSize expected found upId) =
        bprint ("wrong max block size in proposal "%build%
                " (expected "%int%", found "%int%")")
        upId expected found
    build (PollLargeMaxBlockSize maxPossible found upId) =
        bprint ("proposal "%build%" tried to increase max block size"%
                " beyond what is allowed"%
                " (expected max. "%int%", found "%int%")")
        upId maxPossible found
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
    build (PollInternalError msg) =
        bprint ("internal error: "%stext) msg
