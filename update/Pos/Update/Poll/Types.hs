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
       , propStateToEither
       , psProposal
       , psVotes
       , mkUProposalState

         -- * BlockVersion state
       , BlockVersionState (..)
       , bvsIsConfirmed
       , bvsScriptVersion
       , bvsSlotDuration
       , bvsMaxBlockSize

         -- * Rollback
       , PrevValue (..)
       , maybeToPrev
       , USUndo (..)
       , unChangedSVL
       , unChangedPropsL
       , unChangedBVL
       , unLastAdoptedBVL
       , unChangedConfPropsL
       , unPrevProposersL
       , unSlottingDataL

       -- * VoteState
       , StakeholderVotes
       , LocalVotes
       , VoteState (..)
       , canCombineVotes
       , combineVotes
       , isPositiveVote
       , newVoteState
       ) where

import           Universum

import           Control.Lens (makeLensesFor)
import           Data.Default (Default (def))
import qualified Data.Text.Buildable
import           Data.Time.Units (Millisecond)
import           Serokell.Data.Memory.Units (Byte)

import           Pos.Core.Common (ChainDifficulty, Coin, HeaderHash, ScriptVersion, StakeholderId,
                                  mkCoin)
import           Pos.Core.Slotting (EpochIndex, SlotId)
import           Pos.Core.Update (ApplicationName, BlockVersion, BlockVersionModifier (..),
                                  NumSoftwareVersion, SoftwareVersion, UpId, UpdateProposal (..),
                                  UpdateVote)
import           Pos.Crypto (PublicKey)
import           Pos.Slotting.Types (SlottingData)

----------------------------------------------------------------------------
-- VoteState
----------------------------------------------------------------------------

-- | This type represents summary of votes issued by stakeholder.
data VoteState
    = PositiveVote    -- ^ Stakeholder voted once positively.
    | NegativeVote    -- ^ Stakeholder voted once positively.
    | PositiveRevote  -- ^ Stakeholder voted negatively, then positively.
    | NegativeRevote  -- ^ Stakeholder voted positively, then negatively.
    deriving (Show, Generic, Eq)

instance NFData VoteState

instance Buildable VoteState where
    build PositiveVote   = "PositiveVote"
    build NegativeVote   = "NegativeVote"
    build PositiveRevote = "PositiveRevote"
    build NegativeRevote = "NegativeRevote"

-- | Create new VoteState from bool, which is simple vote, not revote.
newVoteState :: Bool -> VoteState
newVoteState True  = PositiveVote
newVoteState False = NegativeVote

isPositiveVote :: VoteState -> Bool
isPositiveVote PositiveVote   = True
isPositiveVote PositiveRevote = True
isPositiveVote _              = False

-- | Check whether given decision is a valid vote if applied to
-- existing vote (which may not exist).
canCombineVotes :: Bool -> Maybe VoteState -> Bool
canCombineVotes _ Nothing                 = True
canCombineVotes True (Just NegativeVote)  = True
canCombineVotes False (Just PositiveVote) = True
canCombineVotes _ _                       = False

-- | Apply decision to given vote (or Nothing). This function returns
-- 'Nothing' if decision can't be applied. 'canCombineVotes' can be
-- used to check whether it will be successful.
combineVotes :: Bool -> Maybe VoteState -> Maybe VoteState
combineVotes decision oldVote =
    case (decision, oldVote) of
        (True, Nothing)            -> Just PositiveVote
        (False, Nothing)           -> Just NegativeVote
        (True, Just NegativeVote)  -> Just PositiveRevote
        (False, Just PositiveVote) -> Just NegativeRevote
        (_, Just _)                -> Nothing

-- | Type alias for set of votes from stakeholders
type StakeholderVotes = HashMap PublicKey VoteState
type LocalVotes = HashMap UpId (HashMap PublicKey UpdateVote)
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
    = PSUndecided { unPSUndecided :: !UndecidedProposalState }
    | PSDecided   { unPSDecided   :: !DecidedProposalState }
      deriving (Eq, Generic, Show)

propStateToEither :: ProposalState -> Either UndecidedProposalState DecidedProposalState
propStateToEither (PSUndecided ups) = Left ups
propStateToEither (PSDecided dps)   = Right dps

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

instance NFData UpsExtra
instance NFData DpsExtra
instance NFData UndecidedProposalState
instance NFData DecidedProposalState
instance NFData ConfirmedProposalState
instance NFData ProposalState

----------------------------------------------------------------------------
-- BlockVersion state
----------------------------------------------------------------------------

-- | State of BlockVersion from update proposal.
data BlockVersionState = BlockVersionState
    { bvsModifier          :: !BlockVersionModifier
    -- ^ 'BlockVersionModifier' associated with this block version.
    , bvsConfirmedEpoch    :: !(Maybe EpochIndex)
    -- ^ Epoch when proposal which generated this block version
    -- was confirmed.
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
    } deriving (Eq, Show, Generic)

-- | Check whether proposal which generated given 'BlockVersionState'
-- is confirmed.
bvsIsConfirmed :: BlockVersionState -> Bool
bvsIsConfirmed = isJust . bvsConfirmedEpoch

bvsScriptVersion :: BlockVersionState -> Maybe ScriptVersion
bvsScriptVersion = bvmScriptVersion . bvsModifier

bvsSlotDuration :: BlockVersionState -> Maybe Millisecond
bvsSlotDuration = bvmSlotDuration . bvsModifier

bvsMaxBlockSize :: BlockVersionState -> Maybe Byte
bvsMaxBlockSize = bvmMaxBlockSize . bvsModifier

----------------------------------------------------------------------------
-- Undo
----------------------------------------------------------------------------

-- | Previous value of something that could be missing.
data PrevValue a = PrevValue a | NoExist
    deriving (Generic, Show, Eq)


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
    , unPrevProposers    :: !(Maybe (HashSet StakeholderId))
    , unSlottingData     :: !(Maybe SlottingData)
    -- ^ 'SlottingData' which should be modified as the result of this rollback
    } deriving (Generic, Show, Eq)


makeLensesFor [ ("unChangedBV", "unChangedBVL")
              , ("unLastAdoptedBV", "unLastAdoptedBVL")
              , ("unChangedProps", "unChangedPropsL")
              , ("unChangedSV", "unChangedSVL")
              , ("unChangedConfProps", "unChangedConfPropsL")
              , ("unPrevProposers", "unPrevProposersL")
              , ("unSlottingData", "unSlottingDataL")
              ]
  ''USUndo

instance Buildable USUndo where
    build _ = "BSUndo"

instance Default USUndo where
    def = USUndo mempty Nothing mempty mempty mempty Nothing Nothing

----------------------------------------------------------------------------
-- NFData instances
----------------------------------------------------------------------------

instance NFData BlockVersionState
instance NFData a => NFData (PrevValue a)
instance NFData USUndo
