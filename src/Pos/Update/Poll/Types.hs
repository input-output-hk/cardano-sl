{-# LANGUAGE TemplateHaskell #-}

-- | Types related to Poll monad.

module Pos.Update.Poll.Types
       (
         -- * Proposal state
         UndecidedProposalState (..)
       , DecidedProposalState (..)
       , ProposalState (..)
       , psProposal
       , psVotes
       , mkUProposalState

         -- * BlockVersion state
       , BlockVersionState (..)

         -- * Poll modifier
       , PollModifier (..)
       , pmNewBVsL
       , pmDelBVsL
       , pmLastAdoptedBVL
       , pmNewConfirmedL
       , pmDelConfirmedL
       , pmNewConfirmedPropsL
       , pmNewActivePropsL
       , pmDelActivePropsL
       , pmNewActivePropsIdxL
       , pmDelActivePropsIdxL

         -- * Verification
       , PollVerFailure (..)

         -- * Rollback
       , PrevValue (..)
       , maybeToPrev
       , USUndo (..)
       , unChangedSVL
       , unChangedPropsL
       , unCreatedNewBSForL
       , unLastAdoptedBVL
       ) where

import           Control.Lens               (makeLensesFor)
import           Data.Default               (Default (def))
import qualified Data.Text.Buildable
import           Data.Time.Units            (Microsecond)
import           Formatting                 (bprint, build, int, sformat, stext, (%))
import           Serokell.Data.Memory.Units (Byte)
import           Universum

import           Pos.Script.Type            (ScriptVersion)
import           Pos.Types.Coin             (coinF)
import           Pos.Types.Types            (ChainDifficulty, Coin, EpochIndex, SlotId,
                                             StakeholderId, mkCoin)
import           Pos.Types.Version          (ApplicationName, BlockVersion,
                                             NumSoftwareVersion, SoftwareVersion)
import           Pos.Update.Core            (StakeholderVotes, UpId, UpdateProposal)

----------------------------------------------------------------------------
-- Proposal State
----------------------------------------------------------------------------

-- | State of UpdateProposal which can't be classified as approved or
-- rejected.
data UndecidedProposalState = UndecidedProposalState
    { upsVotes         :: !StakeholderVotes
      -- ^ Votes given for this proposal.
    , upsProposal      :: !UpdateProposal
      -- ^ Proposal itself.
    , upsSlot          :: !SlotId
      -- ^ SlotId from block in which update was proposed.
    , upsPositiveStake :: !Coin
      -- ^ Total stake of all positive votes.
    , upsNegativeStake :: !Coin
      -- ^ Total stake of all negative votes.
    } deriving (Generic)

-- | State of UpdateProposal which can be classified as approved or
-- rejected.
data DecidedProposalState = DecidedProposalState
    { dpsDecision   :: !Bool
      -- ^ Whether proposal is approved.
    , dpsUndecided  :: !UndecidedProposalState
      -- ^ Corresponding UndecidedProposalState
    , dpsDifficulty :: !(Maybe ChainDifficulty)
      -- ^ Difficulty at which this proposal became approved/rejected.
      --   Can be Nothing in temporary state.
    } deriving (Generic)

-- | State of UpdateProposal.
data ProposalState
    = PSUndecided !UndecidedProposalState
    | PSDecided !DecidedProposalState
      deriving (Generic)

psProposal :: ProposalState -> UpdateProposal
psProposal (PSUndecided ups) = upsProposal ups
psProposal (PSDecided dps)   = upsProposal (dpsUndecided dps)

psVotes :: ProposalState -> StakeholderVotes
psVotes (PSUndecided ups) = upsVotes ups
psVotes (PSDecided dps)   = upsVotes (dpsUndecided dps)

-- | Make UndecidedProposalState from immutable data, i. e. SlotId and
-- UpdateProposal.
mkUProposalState :: SlotId -> UpdateProposal -> UndecidedProposalState
mkUProposalState upsSlot upsProposal =
    UndecidedProposalState
    { upsVotes = mempty , upsPositiveStake = mkCoin 0
    , upsNegativeStake = mkCoin 0
    , ..
    }

----------------------------------------------------------------------------
-- BlockVersion state
----------------------------------------------------------------------------

-- | State of BlockVersion from update proposal.
data BlockVersionState = BlockVersionState
    { bvsScriptVersion :: !ScriptVersion
    -- ^ Script version associated with this block version.
    , bvsIsConfirmed   :: !Bool
    -- ^ Whether proposal with this block version is confirmed.
    , bvsSlotDuration  :: !Microsecond
    , bvsMaxBlockSize  :: !Byte
    }

----------------------------------------------------------------------------
-- Modifier
----------------------------------------------------------------------------

-- | PollModifier is used in verification. It represents operation which
-- one should apply to global state to obtain result of application of
-- MemPool or blocks which are verified.
data PollModifier = PollModifier
    { pmNewBVs            :: !(HashMap BlockVersion BlockVersionState)
    , pmDelBVs            :: !(HashSet BlockVersion)
    , pmLastAdoptedBV     :: !(Maybe BlockVersion)
    , pmNewConfirmed      :: !(HashMap ApplicationName NumSoftwareVersion)
    , pmDelConfirmed      :: !(HashSet ApplicationName)
    , pmNewConfirmedProps :: !(HashMap NumSoftwareVersion UpdateProposal)
    , pmNewActiveProps    :: !(HashMap UpId ProposalState)
    , pmDelActiveProps    :: !(HashSet UpId)
    , pmNewActivePropsIdx :: !(HashMap ApplicationName UpId)
    , pmDelActivePropsIdx :: !(HashMap ApplicationName UpId)
    }

makeLensesFor [ ("pmNewBVs", "pmNewBVsL")
              , ("pmDelBVs", "pmDelBVsL")
              , ("pmLastAdoptedBV", "pmLastAdoptedBVL")
              , ("pmNewConfirmed", "pmNewConfirmedL")
              , ("pmDelConfirmed", "pmDelConfirmedL")
              , ("pmNewConfirmedProps", "pmNewConfirmedPropsL")
              , ("pmNewActiveProps", "pmNewActivePropsL")
              , ("pmDelActiveProps", "pmDelActivePropsL")
              , ("pmNewActivePropsIdx", "pmNewActivePropsIdxL")
              , ("pmDelActivePropsIdx", "pmDelActivePropsIdxL")
              ]
  ''PollModifier

----------------------------------------------------------------------------
-- Verification failures
----------------------------------------------------------------------------

-- To be extended for sure.
-- | PollVerificationFailure represents all possible errors which can
-- appear in Poll data verification.
data PollVerFailure
    = PollWrongScriptVersion { pwsvExpected :: !ScriptVersion
                             , pwsvFound    :: !ScriptVersion
                             , pwsvUpId     :: !UpId}
    -- | Slot duration for this block version is already known and the one we
    -- saw doesn't match it
    | PollWrongSlotDuration { pwsdExpected :: !Microsecond
                            , pwsdFound    :: !Microsecond
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
    | PollSmallProposalStake { pspsThreshold :: !Coin
                            ,  pspsActual    :: !Coin
                            ,  pspsUpId      :: !UpId}
    | PollNotRichman { pnrStakeholder :: !StakeholderId
                    ,  pnrThreshold   :: !Coin
                    ,  pnrStake       :: !(Maybe Coin)}
    | PollUnknownProposal { pupStakeholder :: !StakeholderId
                         ,  pupProposal    :: !UpId}
    | PollUnknownStakes !EpochIndex
    | Poll2ndActiveProposal !SoftwareVersion
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
    build (Poll2ndActiveProposal sv) =
        bprint ("there is already active proposal for given application, "%
                "software version is: "%build)
        sv
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
    build (PollInternalError msg) =
        bprint ("internal error: "%stext) msg

----------------------------------------------------------------------------
-- Undo
----------------------------------------------------------------------------

-- | Previous value of something that could be missing.
data PrevValue a = PrevValue a | NoExist

maybeToPrev :: Maybe a -> PrevValue a
maybeToPrev (Just x) = PrevValue x
maybeToPrev Nothing  = NoExist

-- | Data necessary to unapply US data.
data USUndo = USUndo
    { unCreatedNewBSFor :: !(Maybe BlockVersion)
    , unLastAdoptedBV   :: !(Maybe BlockVersion)
    , unChangedProps    :: !(HashMap UpId (PrevValue ProposalState))
    , unChangedSV       :: !(HashMap ApplicationName (PrevValue NumSoftwareVersion))
    }

makeLensesFor [ ("unCreatedNewBSFor", "unCreatedNewBSForL")
              , ("unLastAdoptedBV", "unLastAdoptedBVL")
              , ("unChangedProps", "unChangedPropsL")
              , ("unChangedSV", "unChangedSVL")
              ]
  ''USUndo

instance Buildable USUndo where
    build _ = "BSUndo"

instance Default USUndo where
    def = USUndo Nothing Nothing mempty mempty
