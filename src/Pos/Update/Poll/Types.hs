{-# LANGUAGE TemplateHaskell #-}

-- | Types related to Poll monad.

module Pos.Update.Poll.Types
       (
         -- * Proposal state
         UndecidedProposalState (..)
       , DecidedProposalState (..)
       , ProposalState (..)
       , psProposal
       , mkUProposalState
       , voteToUProposalState

         -- * Poll modifier
       , PollModifier (..)
       , pmNewScriptVersionsL
       , pmLastAdoptedPVL
       , pmNewConfirmedL
       , pmNewActivePropsL
       , pmDelActivePropsL
       , pmNewActivePropsIdxL
       , pmDelActivePropsIdxL

         -- * Verification
       , PollVerFailure (..)
       , USUndo (..)
       ) where

import           Control.Lens        (makeLensesFor)
import           Data.Default        (Default (def))
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Buildable
import           Universum

import           Pos.Crypto          (PublicKey)
import           Pos.Script.Type     (ScriptVersion)
import           Pos.Types.Coin      (unsafeAddCoin)
import           Pos.Types.Types     (ChainDifficulty, Coin, SlotId, mkCoin)
import           Pos.Types.Version   (ApplicationName, NumSoftwareVersion,
                                      ProtocolVersion)
import           Pos.Update.Core     (StakeholderVotes, UpId, UpdateProposal,
                                      combineVotes)

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
    , dpsProposal   :: !UpdateProposal
      -- ^ Proposal itself.
    , dpsDifficulty :: !ChainDifficulty
      -- ^ Difficulty at which this proposal became approved/rejected.
    } deriving (Generic)

-- | State of UpdateProposal.
data ProposalState
    = PSUndecided !UndecidedProposalState
    | PSDecided !DecidedProposalState
      deriving (Generic)

psProposal :: ProposalState -> UpdateProposal
psProposal (PSUndecided ups) = upsProposal ups
psProposal (PSDecided dps)   = dpsProposal dps

-- | Make UndecidedProposalState from immutable data, i. e. SlotId and
-- UpdateProposal.
mkUProposalState :: SlotId -> UpdateProposal -> UndecidedProposalState
mkUProposalState upsSlot upsProposal =
    UndecidedProposalState
    { upsVotes = mempty , upsPositiveStake = mkCoin 0
    , upsNegativeStake = mkCoin 0
    , ..
    }

-- | Apply vote to UndecidedProposalState, thus modifing mutable data,
-- i. e. votes and stakes.
voteToUProposalState ::
    PublicKey -> Coin -> Bool -> UndecidedProposalState -> UndecidedProposalState
voteToUProposalState voter stake decision UndecidedProposalState {..} =
    UndecidedProposalState
    { upsVotes = HM.alter (Just . combineVotes decision) voter upsVotes
    , upsPositiveStake = newPositiveStake
    , upsNegativeStake = newNegativeStake
    , ..
    }
  where
    newPositiveStake
        | decision = upsPositiveStake `unsafeAddCoin` stake
        | otherwise = upsPositiveStake
    newNegativeStake
        | decision = upsNegativeStake
        | otherwise = upsNegativeStake `unsafeAddCoin` stake

----------------------------------------------------------------------------
-- Modifier
----------------------------------------------------------------------------

-- | PollModifier is used in verification. It represents operation which
-- one should apply to global state to obtain result of application of
-- MemPool or blocks which are verified.
data PollModifier = PollModifier
    { pmNewScriptVersions :: !(HashMap ProtocolVersion ScriptVersion)
    , pmLastAdoptedPV     :: !(Maybe ProtocolVersion)
    , pmNewConfirmed      :: !(HashMap ApplicationName NumSoftwareVersion)
    , pmNewActiveProps    :: !(HashMap UpId ProposalState)
    , pmDelActiveProps    :: !(HashSet UpId)
    , pmNewActivePropsIdx :: !(HashMap ApplicationName UpId)
    , pmDelActivePropsIdx :: !(HashSet ApplicationName)
    }

makeLensesFor [ ("pmNewScriptVersions", "pmNewScriptVersionsL")
              , ("pmLastAdoptedPV", "pmLastAdoptedPVL")
              , ("pmNewConfirmed", "pmNewConfirmedL")
              , ("pmNewActiveProps", "pmNewActivePropsL")
              , ("pmDelActiveProps", "pmDelActivePropsL")
              , ("pmNewActivePropsIdx", "pmNewActivePropsIdxL")
              , ("pmDelActivePropsIdx", "pmDelActivePropsIdxL")
              ]
  ''PollModifier

instance Default PollModifier where
    def =
        PollModifier
        { pmNewScriptVersions = mempty
        , pmLastAdoptedPV = Nothing
        , pmNewConfirmed = mempty
        , pmNewActiveProps = mempty
        , pmDelActiveProps = mempty
        , pmNewActivePropsIdx = mempty
        , pmDelActivePropsIdx = mempty
        }

-- To be extended for sure.
-- | PollVerificationFailure represents all possible errors which can
-- appear in Poll data verification.
data PollVerFailure
    = PollWrongScriptVersion { pwsvExpected :: !ScriptVersion
                            ,  pwsvFound    :: !ScriptVersion}
    | PollSmallProposalStake { pspsThreshold :: !Coin
                            ,  pspsActual    :: !Coin}

-- To be implemented for sure.
instance Buildable PollVerFailure where
    build = notImplemented

-- To be extended for sure.
data USUndo = USUndo

instance Buildable USUndo where
    build _ = ""

instance Default USUndo where
    def = USUndo
