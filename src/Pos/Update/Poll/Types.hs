{-# LANGUAGE TemplateHaskell #-}

-- | Types related to Poll monad.

module Pos.Update.Poll.Types
       (
         -- * Proposal state
         UndecidedProposalState (..)
       , DecidedProposalState (..)
       , ProposalState (..)
       , UpsExtra (..)
       , DpsExtra (..)
       , ConfirmedProposalState (..)
       , cpsBlockVersion
       , cpsSoftwareVersion
       , psProposal
       , psVotes
       , mkUProposalState

         -- * BlockVersion state
       , BlockVersionState (..)
       , bvsScriptVersion
       , bvsSlotDuration
       , bvsMaxBlockSize

         -- * Poll modifier
       , PollModifier (..)
       , pmNewBVsL
       , pmDelBVsL
       , pmAdoptedBVFullL
       , pmNewConfirmedL
       , pmDelConfirmedL
       , pmNewConfirmedPropsL
       , pmDelConfirmedPropsL
       , pmNewActivePropsL
       , pmDelActivePropsL
       , pmNewActivePropsIdxL
       , pmDelActivePropsIdxL
       , pmSlottingDataL

         -- * Rollback
       , PrevValue (..)
       , maybeToPrev
       , USUndo (..)
       , unChangedSVL
       , unChangedPropsL
       , unChangedBVL
       , unLastAdoptedBVL
       , unChangedConfPropsL
       ) where

import           Control.Lens               (makeLensesFor)
import           Data.Default               (Default (def))
import qualified Data.Text.Buildable
import           Data.Time.Units            (Millisecond)
import           Serokell.Data.Memory.Units (Byte)
import           Universum

import           Pos.Slotting.Types         (SlottingData)
import           Pos.Types.Core             (ChainDifficulty, Coin, HeaderHash, SlotId,
                                             StakeholderId, mkCoin)
import           Pos.Types.Script           (ScriptVersion)
import           Pos.Types.Version          (ApplicationName, BlockVersion,
                                             NumSoftwareVersion, SoftwareVersion)
import           Pos.Update.Core            (BlockVersionData (..), StakeholderVotes,
                                             UpId, UpdateProposal (..))

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
    , upsExtra         :: !(Maybe UpsExtra)
      -- ^ Extra data
    } deriving (Show, Generic, Eq)

-- | Extra data required by wallet, stored in UndecidedProposalState
data UpsExtra = UpsExtra
    { ueProposedBlk :: !HeaderHash
    -- ^ Block in which this update was proposed
    } deriving (Show, Generic, Eq)

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
    , dpsExtra      :: !(Maybe DpsExtra)
      -- ^ Extra data
    } deriving (Show, Generic, Eq)

-- | Extra data required by wallet, stored in DecidedProposalState.
data DpsExtra = DpsExtra
    { deDecidedBlk :: !HeaderHash
      -- ^ HeaderHash  of block in which this update was approved/rejected
    , deImplicit   :: !Bool
      -- ^ Which way we approve/reject this update proposal: implicit or explicit
    } deriving (Show, Generic, Eq)

-- | Information about confirmed proposals stored in DB.
data ConfirmedProposalState = ConfirmedProposalState
    { cpsUpdateProposal :: !UpdateProposal
    , cpsImplicit       :: !Bool
    , cpsProposed       :: !HeaderHash
    , cpsDecided        :: !HeaderHash
    , cpsConfirmed      :: !HeaderHash
    , cpsAdopted        :: !(Maybe HeaderHash)
    , cpsVotes          :: !StakeholderVotes
    , cpsPositiveStake  :: !Coin
    , cpsNegativeStake  :: !Coin
    } deriving (Show, Generic, Eq)

-- | Get 'BlockVersion' from 'ConfirmedProposalState'.
cpsBlockVersion :: ConfirmedProposalState -> BlockVersion
cpsBlockVersion = upBlockVersion . cpsUpdateProposal

-- | Get 'SoftwareVersion' from 'ConfirmedProposalState'.
cpsSoftwareVersion :: ConfirmedProposalState -> SoftwareVersion
cpsSoftwareVersion = upSoftwareVersion . cpsUpdateProposal

-- | State of UpdateProposal.
data ProposalState
    = PSUndecided !UndecidedProposalState
    | PSDecided !DecidedProposalState
      deriving (Generic, Show)

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
    , upsExtra = Nothing
    , ..
    }

----------------------------------------------------------------------------
-- BlockVersion state
----------------------------------------------------------------------------

-- | State of BlockVersion from update proposal.
data BlockVersionState = BlockVersionState
    { bvsData              :: !BlockVersionData
    -- ^ 'BlockVersioData' associated with this block version.
    , bvsIsConfirmed       :: !Bool
    -- ^ Whether proposal with this block version is confirmed.
    , bvsIssuersStable     :: !(HashSet StakeholderId)
    -- ^ Identifiers of stakeholders which issued stable blocks with this
    -- 'BlockVersion'. Stability is checked by the same rules as used in LRC.
    -- That is, 'SlotId' is considered. If block is created after crucial slot
    -- of 'i'-th epoch, it is not stable when 'i+1'-th epoch starts.
    , bvsIssuersUnstable   :: !(HashSet StakeholderId)
    -- ^ Identifiers of stakeholders which issued unstable blocks with
    -- this 'BlockVersion'. See description of 'bvsIssuersStable' for
    -- details.
    , bvsLastBlockStable   :: !(Maybe HeaderHash)
    -- ^ Identifier of last block which modified set of 'bvsIssuersStable'.
    , bvsLastBlockUnstable :: !(Maybe HeaderHash)
    -- ^ Identifier of last block which modified set of 'bvsIssuersUnstable'.
    } deriving (Show)

bvsScriptVersion :: BlockVersionState -> ScriptVersion
bvsScriptVersion = bvdScriptVersion . bvsData

bvsSlotDuration :: BlockVersionState -> Millisecond
bvsSlotDuration = bvdSlotDuration . bvsData

bvsMaxBlockSize :: BlockVersionState -> Byte
bvsMaxBlockSize = bvdMaxBlockSize . bvsData

----------------------------------------------------------------------------
-- Modifier
----------------------------------------------------------------------------

-- | PollModifier is used in verification. It represents operation which
-- one should apply to global state to obtain result of application of
-- MemPool or blocks which are verified.
data PollModifier = PollModifier
    { pmNewBVs            :: !(HashMap BlockVersion BlockVersionState)
    , pmDelBVs            :: !(HashSet BlockVersion)
    , pmAdoptedBVFull     :: !(Maybe (BlockVersion, BlockVersionData))
    , pmNewConfirmed      :: !(HashMap ApplicationName NumSoftwareVersion)
    , pmDelConfirmed      :: !(HashSet ApplicationName)
    , pmNewConfirmedProps :: !(HashMap SoftwareVersion ConfirmedProposalState)
    , pmDelConfirmedProps :: !(HashSet SoftwareVersion)
    , pmNewActiveProps    :: !(HashMap UpId ProposalState)
    , pmDelActiveProps    :: !(HashSet UpId)
    , pmNewActivePropsIdx :: !(HashMap ApplicationName UpId)
    , pmDelActivePropsIdx :: !(HashMap ApplicationName UpId)
    , pmSlottingData      :: !(Maybe SlottingData)
    } deriving (Show)

makeLensesFor [ ("pmNewBVs", "pmNewBVsL")
              , ("pmDelBVs", "pmDelBVsL")
              , ("pmAdoptedBVFull", "pmAdoptedBVFullL")
              , ("pmNewConfirmed", "pmNewConfirmedL")
              , ("pmDelConfirmed", "pmDelConfirmedL")
              , ("pmNewConfirmedProps", "pmNewConfirmedPropsL")
              , ("pmDelConfirmedProps", "pmDelConfirmedPropsL")
              , ("pmNewActiveProps", "pmNewActivePropsL")
              , ("pmDelActiveProps", "pmDelActivePropsL")
              , ("pmNewActivePropsIdx", "pmNewActivePropsIdxL")
              , ("pmDelActivePropsIdx", "pmDelActivePropsIdxL")
              , ("pmSlottingData", "pmSlottingDataL")
              ]
  ''PollModifier

----------------------------------------------------------------------------
-- Undo
----------------------------------------------------------------------------

-- | Previous value of something that could be missing.
data PrevValue a = PrevValue a | NoExist
    deriving (Show)

maybeToPrev :: Maybe a -> PrevValue a
maybeToPrev (Just x) = PrevValue x
maybeToPrev Nothing  = NoExist

-- | Data necessary to unapply US data.
data USUndo = USUndo
    { unChangedBV        :: !(HashMap BlockVersion (PrevValue BlockVersionState))
    , unLastAdoptedBV    :: !(Maybe BlockVersion)
    , unChangedProps     :: !(HashMap UpId (PrevValue ProposalState))
    , unChangedSV        :: !(HashMap ApplicationName (PrevValue NumSoftwareVersion))
    , unChangedConfProps :: !(HashMap SoftwareVersion (PrevValue ConfirmedProposalState))
    }
    deriving (Show)

makeLensesFor [ ("unChangedBV", "unChangedBVL")
              , ("unLastAdoptedBV", "unLastAdoptedBVL")
              , ("unChangedProps", "unChangedPropsL")
              , ("unChangedSV", "unChangedSVL")
              , ("unChangedConfProps", "unChangedConfPropsL")
              ]
  ''USUndo

instance Buildable USUndo where
    build _ = "BSUndo"

instance Default USUndo where
    def = USUndo mempty Nothing mempty mempty mempty
