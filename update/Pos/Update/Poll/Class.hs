{-# LANGUAGE TypeFamilies #-}

-- | Type classes for Poll abstraction.

module Pos.Update.Poll.Class
       ( MonadPollRead (..)
       , MonadPoll (..)
       ) where

import           Universum

import           Control.Monad.Trans (MonadTrans)
import           System.Wlog (WithLogger)

import           Pos.Core (ApplicationName, BlockVersion, BlockVersionData, ChainDifficulty, Coin,
                           EpochIndex, NumSoftwareVersion, SlotId,
                           SoftwareVersion, StakeholderId)
import           Pos.Core.Update (UpId)
import           Pos.Slotting.Types (SlottingData)
import           Pos.Update.Poll.Types (BlockVersionState, ConfirmedProposalState,
                                        DecidedProposalState, ProposalState, UndecidedProposalState)

----------------------------------------------------------------------------
-- Read-only
----------------------------------------------------------------------------

-- | Type class which provides function necessary for read-only
-- verification of US data.
class (Monad m, WithLogger m) => MonadPollRead m where
    getBVState :: BlockVersion -> m (Maybe BlockVersionState)
    -- ^ Retrieve state of given block version.
    getProposedBVs :: m [BlockVersion]
    -- ^ Retrieve all proposed block versions.
    getEpochProposers :: m (HashSet StakeholderId)
    -- ^ Retrieve all stakeholders who proposed proposals in the current epoch.
    getCompetingBVStates :: m [(BlockVersion, BlockVersionState)]
    -- ^ Get all competing 'BlockVersion's and their states.
    getAdoptedBVFull :: m (BlockVersion, BlockVersionData)
    -- ^ Retrieve last adopted block version and its state.
    getLastConfirmedSV :: ApplicationName -> m (Maybe NumSoftwareVersion)
    -- ^ Get numeric component of last confirmed version of application
    getProposal :: UpId -> m (Maybe ProposalState)
    -- ^ Get active proposal
    getProposalsByApp :: ApplicationName -> m [ProposalState]
    -- ^ Get active proposals for the specified application.
    getConfirmedProposals :: m [ConfirmedProposalState]
    -- ^ Get all known confirmed proposals.
    getEpochTotalStake :: EpochIndex -> m (Maybe Coin)
    -- ^ Get total stake from distribution corresponding to given epoch
    getRichmanStake :: EpochIndex -> StakeholderId -> m (Maybe Coin)
    -- ^ Get stake of ricmhan corresponding to given epoch (if she is
    -- really rich)
    getOldProposals :: SlotId -> m [UndecidedProposalState]
    -- ^ Get all proposals which are in undecided state and were
    -- included into block with slot less than or equal to given.
    getDeepProposals :: ChainDifficulty -> m [DecidedProposalState]
    -- ^ Get all proposals which are in decided state and become
    -- decided deeper than given 'ChainDifficulty'.
    getBlockIssuerStake :: EpochIndex -> StakeholderId -> m (Maybe Coin)
    -- ^ Get stake of issuer of one of the blocks created so far using
    -- stake distribution which is stable in given epoch.
    -- Only issuer of stable block can be passed to this function, otherwise
    -- 'Nothing' will be returned.
    getSlottingData :: m SlottingData
    -- ^ Get most recent 'SlottingData'.

    getAdoptedBV :: m BlockVersion
    getAdoptedBV = fst <$> getAdoptedBVFull

    getAdoptedBVData :: m BlockVersionData
    getAdoptedBVData = snd <$> getAdoptedBVFull

instance {-# OVERLAPPABLE #-}
    (MonadPollRead m, MonadTrans t, Monad (t m), WithLogger (t m)) =>
        MonadPollRead (t m)
  where
    getBVState = lift . getBVState
    getProposedBVs = lift getProposedBVs
    getEpochProposers = lift getEpochProposers
    getCompetingBVStates = lift getCompetingBVStates
    getAdoptedBVFull = lift getAdoptedBVFull
    getLastConfirmedSV = lift . getLastConfirmedSV
    getProposal = lift . getProposal
    getProposalsByApp = lift . getProposalsByApp
    getConfirmedProposals = lift getConfirmedProposals
    getEpochTotalStake = lift . getEpochTotalStake
    getRichmanStake e = lift . getRichmanStake e
    getOldProposals = lift . getOldProposals
    getDeepProposals = lift . getDeepProposals
    getBlockIssuerStake e = lift . getBlockIssuerStake e
    getSlottingData = lift getSlottingData


----------------------------------------------------------------------------
-- Writeable
----------------------------------------------------------------------------

-- | Type class which provides function necessary for verification of
-- US data with ability to modify state.
class MonadPollRead m => MonadPoll m where
    putBVState :: BlockVersion -> BlockVersionState -> m ()
    -- ^ Put state of BlockVersion overriding if it exists.
    delBVState :: BlockVersion -> m ()
    -- ^ Delete BlockVersion and associated state.
    setAdoptedBV :: BlockVersion -> m ()
    -- ^ Set last adopted block version. State is taken from competing states.
    setLastConfirmedSV :: SoftwareVersion -> m ()
    -- ^ Set last confirmed version of application.
    delConfirmedSV :: ApplicationName -> m ()
    -- ^ Del last confirmed version of application.
    addConfirmedProposal :: ConfirmedProposalState -> m ()
    -- ^ Add new confirmed update proposal.
    delConfirmedProposal :: SoftwareVersion -> m ()
    -- ^ Del confirmed update proposal (for rollback only).
    insertActiveProposal :: ProposalState -> m ()
    -- ^ Add new active proposal with its state.
    deactivateProposal :: UpId -> m ()
    -- ^ Delete active proposal given its name and identifier.
    setSlottingData :: SlottingData -> m ()
    -- ^ Set most recent 'SlottingData'.
    setEpochProposers :: HashSet StakeholderId -> m ()
    -- ^ Set proposers.

instance {-# OVERLAPPABLE #-}
    (MonadPoll m, MonadTrans t, Monad (t m), WithLogger (t m)) =>
        MonadPoll (t m)
  where
    putBVState pv = lift . putBVState pv
    delBVState = lift . delBVState
    setAdoptedBV = lift . setAdoptedBV
    setLastConfirmedSV = lift . setLastConfirmedSV
    delConfirmedSV = lift . delConfirmedSV
    addConfirmedProposal = lift . addConfirmedProposal
    delConfirmedProposal = lift . delConfirmedProposal
    insertActiveProposal = lift . insertActiveProposal
    deactivateProposal = lift . deactivateProposal
    setSlottingData = lift . setSlottingData
    setEpochProposers = lift . setEpochProposers
