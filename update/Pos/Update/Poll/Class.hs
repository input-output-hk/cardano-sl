{-# LANGUAGE TypeFamilies #-}

-- | Type classes for Poll abstraction.

module Pos.Update.Poll.Class
       ( MonadPollRead (..)
       , MonadPoll (..)
       ) where

import           Control.Monad.Trans   (MonadTrans)
import           System.Wlog           (WithLogger)
import           Universum

import           Pos.Core              (ApplicationName, BlockVersion, ChainDifficulty,
                                        Coin, EpochIndex, NumSoftwareVersion, SlotId,
                                        SoftwareVersion, StakeholderId)
import           Pos.Slotting.Types    (SlottingData)
import           Pos.Update.Core       (BlockVersionData, UpId)
import           Pos.Update.Poll.Types (BlockVersionState, ConfirmedProposalState,
                                        DecidedProposalState, ProposalState,
                                        UndecidedProposalState)

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

    -- | Default implementations for 'MonadTrans'.
    default getBVState
        :: (MonadTrans t, MonadPollRead m', t m' ~ m) =>
        BlockVersion -> m (Maybe BlockVersionState)
    getBVState = lift . getBVState

    default getProposedBVs
        :: (MonadTrans t, MonadPollRead m', t m' ~ m) => m [BlockVersion]
    getProposedBVs = lift getProposedBVs

    default getEpochProposers
        :: (MonadTrans t, MonadPollRead m', t m' ~ m) => m (HashSet StakeholderId)
    getEpochProposers = lift getEpochProposers

    default getCompetingBVStates
        :: (MonadTrans t, MonadPollRead m', t m' ~ m) =>
        m [(BlockVersion, BlockVersionState)]
    getCompetingBVStates = lift getCompetingBVStates

    default getAdoptedBVFull
        :: (MonadTrans t, MonadPollRead m', t m' ~ m) => m (BlockVersion, BlockVersionData)
    getAdoptedBVFull = lift getAdoptedBVFull

    default getLastConfirmedSV
        :: (MonadTrans t, MonadPollRead m', t m' ~ m) =>
        ApplicationName -> m (Maybe Word32)
    getLastConfirmedSV = lift . getLastConfirmedSV

    default getProposal
        :: (MonadTrans t, MonadPollRead m', t m' ~ m) =>
        UpId -> m (Maybe ProposalState)
    getProposal = lift . getProposal

    default getProposalsByApp
        :: (MonadTrans t, MonadPollRead m', t m' ~ m) =>
        ApplicationName -> m [ProposalState]
    getProposalsByApp = lift . getProposalsByApp

    default getConfirmedProposals
        :: (MonadTrans t, MonadPollRead m', t m' ~ m) =>
        m [ConfirmedProposalState]
    getConfirmedProposals = lift getConfirmedProposals

    default getEpochTotalStake
        :: (MonadTrans t, MonadPollRead m', t m' ~ m) =>
        EpochIndex -> m (Maybe Coin)
    getEpochTotalStake = lift . getEpochTotalStake

    default getRichmanStake
        :: (MonadTrans t, MonadPollRead m', t m' ~ m) =>
        EpochIndex -> StakeholderId -> m (Maybe Coin)
    getRichmanStake e = lift . getRichmanStake e

    default getOldProposals
        :: (MonadTrans t, MonadPollRead m', t m' ~ m) =>
        SlotId -> m [UndecidedProposalState]
    getOldProposals = lift . getOldProposals

    default getDeepProposals
        :: (MonadTrans t, MonadPollRead m', t m' ~ m) =>
        ChainDifficulty -> m [DecidedProposalState]
    getDeepProposals = lift . getDeepProposals

    default getBlockIssuerStake
        :: (MonadTrans t, MonadPollRead m', t m' ~ m) =>
        EpochIndex -> StakeholderId -> m (Maybe Coin)
    getBlockIssuerStake e = lift . getBlockIssuerStake e

    default getSlottingData
        :: (MonadTrans t, MonadPollRead m', t m' ~ m) => m SlottingData
    getSlottingData = lift getSlottingData

instance {-# OVERLAPPABLE #-}
    (MonadPollRead m, MonadTrans t, Monad (t m), WithLogger (t m)) =>
        MonadPollRead (t m)

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

    -- | Default implementations for 'MonadTrans'.
    default putBVState
        :: (MonadTrans t, MonadPoll m', t m' ~ m) => BlockVersion -> BlockVersionState -> m ()
    putBVState pv = lift . putBVState pv

    default delBVState
        :: (MonadTrans t, MonadPoll m', t m' ~ m) => BlockVersion -> m ()
    delBVState = lift . delBVState

    default setAdoptedBV
        :: (MonadTrans t, MonadPoll m', t m' ~ m) => BlockVersion -> m ()
    setAdoptedBV = lift . setAdoptedBV

    default setLastConfirmedSV
        :: (MonadTrans t, MonadPoll m', t m' ~ m) => SoftwareVersion -> m ()
    setLastConfirmedSV = lift . setLastConfirmedSV

    default delConfirmedSV
        :: (MonadTrans t, MonadPoll m', t m' ~ m) => ApplicationName -> m ()
    delConfirmedSV = lift . delConfirmedSV

    default addConfirmedProposal
        :: (MonadTrans t, MonadPoll m', t m' ~ m) => ConfirmedProposalState -> m ()
    addConfirmedProposal = lift . addConfirmedProposal

    default delConfirmedProposal
        :: (MonadTrans t, MonadPoll m', t m' ~ m) => SoftwareVersion -> m ()
    delConfirmedProposal = lift . delConfirmedProposal

    default insertActiveProposal
        :: (MonadTrans t, MonadPoll m', t m' ~ m) => ProposalState -> m ()
    insertActiveProposal = lift . insertActiveProposal

    default deactivateProposal
        :: (MonadTrans t, MonadPoll m', t m' ~ m) => UpId -> m ()
    deactivateProposal = lift . deactivateProposal

    default setSlottingData
        :: (MonadTrans t, MonadPoll m', t m' ~ m) => SlottingData -> m ()
    setSlottingData = lift . setSlottingData

    default setEpochProposers
        :: (MonadTrans t, MonadPoll m', t m' ~ m) => HashSet StakeholderId -> m ()
    setEpochProposers = lift . setEpochProposers

instance {-# OVERLAPPABLE #-}
    (MonadPoll m, MonadTrans t, Monad (t m), WithLogger (t m)) =>
        MonadPoll (t m)
