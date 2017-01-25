{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Type classes for Poll abstraction.

module Pos.Update.Poll.Class
       ( MonadPollRead (..)
       , MonadPoll (..)
       ) where

import           Control.Monad.Except  (ExceptT)
import           Control.Monad.Trans   (MonadTrans)
import           Universum

import           Pos.Types             (ApplicationName, BlockVersion, ChainDifficulty,
                                        Coin, EpochIndex, NumSoftwareVersion, SlotId,
                                        SoftwareVersion, StakeholderId)
import           Pos.Update.Core       (UpId)
import           Pos.Update.Poll.Types (BlockVersionState, ConfirmedProposalState,
                                        DecidedProposalState, ProposalState,
                                        UndecidedProposalState)

----------------------------------------------------------------------------
-- Read-only
----------------------------------------------------------------------------

-- | Type class which provides function necessary for read-only
-- verification of US data.
class Monad m => MonadPollRead m where
    getBVState :: BlockVersion -> m (Maybe BlockVersionState)
    -- ^ Retrieve state of given block version.
    getProposedBVs :: m [BlockVersion]
    -- ^ Retrieve all proposed block version.
    getConfirmedBVStates :: m [(BlockVersion, BlockVersionState)]
    -- ^ Get all confirmed 'BlockVersion's and their states.
    getLastBVState :: m BlockVersionState
    -- ^ Retrieve state of last adopted block version.
    getLastAdoptedBV :: m BlockVersion
    -- ^ Get last adopted block version.
    getLastConfirmedSV :: ApplicationName -> m (Maybe NumSoftwareVersion)
    -- ^ Get numeric component of last confirmed version of application
    hasActiveProposal :: ApplicationName -> m Bool
    -- ^ Check if given application has an active (non-confirmed) proposal
    getProposal :: UpId -> m (Maybe ProposalState)
    -- ^ Get active proposal
    getConfirmedProposals :: m [ConfirmedProposalState]
    -- ^ Get all known confirmed proposals.
    getEpochTotalStake :: EpochIndex -> m (Maybe Coin)
    -- ^ Get total stake from distribution corresponding to give epoch
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

    -- | Default implementations for 'MonadTrans'.
    default getBVState
        :: (MonadTrans t, MonadPollRead m', t m' ~ m) =>
        BlockVersion -> m (Maybe BlockVersionState)
    getBVState = lift . getBVState

    default getProposedBVs
        :: (MonadTrans t, MonadPollRead m', t m' ~ m) => m [BlockVersion]
    getProposedBVs = lift getProposedBVs

    default getConfirmedBVStates
        :: (MonadTrans t, MonadPollRead m', t m' ~ m) =>
        m [(BlockVersion, BlockVersionState)]
    getConfirmedBVStates = lift getConfirmedBVStates

    default getLastBVState
        :: (MonadTrans t, MonadPollRead m', t m' ~ m) => m BlockVersionState
    getLastBVState = lift getLastBVState

    default getLastAdoptedBV
        :: (MonadTrans t, MonadPollRead m', t m' ~ m) => m BlockVersion
    getLastAdoptedBV = lift getLastAdoptedBV

    default getLastConfirmedSV
        :: (MonadTrans t, MonadPollRead m', t m' ~ m) =>
        ApplicationName -> m (Maybe Word32)
    getLastConfirmedSV = lift . getLastConfirmedSV

    default hasActiveProposal
        :: (MonadTrans t, MonadPollRead m', t m' ~ m) =>
        ApplicationName -> m Bool
    hasActiveProposal = lift . hasActiveProposal

    default getProposal
        :: (MonadTrans t, MonadPollRead m', t m' ~ m) =>
        UpId -> m (Maybe ProposalState)
    getProposal = lift . getProposal

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

instance MonadPollRead m => MonadPollRead (ReaderT s m)
instance MonadPollRead m => MonadPollRead (StateT s m)
instance MonadPollRead m => MonadPollRead (ExceptT s m)

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
    setLastAdoptedBV :: BlockVersion -> m ()
    -- ^ Set last adopted protocol version.
    setLastConfirmedSV :: SoftwareVersion -> m ()
    -- ^ Set last confirmed version of application.
    delConfirmedSV :: ApplicationName -> m ()
    -- ^ Del last confirmed version of application.
    addConfirmedProposal :: ConfirmedProposalState -> m ()
    -- ^ Add new confirmed update proposal.
    addActiveProposal :: ProposalState -> m ()
    -- ^ Add new active proposal with its state.
    deactivateProposal :: UpId -> m ()
    -- ^ Delete active proposal given its name and identifier.

    -- | Default implementations for 'MonadTrans'.
    default putBVState
        :: (MonadTrans t, MonadPoll m', t m' ~ m) => BlockVersion -> BlockVersionState -> m ()
    putBVState pv = lift . putBVState pv

    default delBVState
        :: (MonadTrans t, MonadPoll m', t m' ~ m) => BlockVersion -> m ()
    delBVState = lift . delBVState

    default setLastAdoptedBV
        :: (MonadTrans t, MonadPoll m', t m' ~ m) => BlockVersion -> m ()
    setLastAdoptedBV = lift . setLastAdoptedBV

    default setLastConfirmedSV
        :: (MonadTrans t, MonadPoll m', t m' ~ m) => SoftwareVersion -> m ()
    setLastConfirmedSV = lift . setLastConfirmedSV

    default delConfirmedSV
        :: (MonadTrans t, MonadPoll m', t m' ~ m) => ApplicationName -> m ()
    delConfirmedSV = lift . delConfirmedSV

    default addConfirmedProposal
        :: (MonadTrans t, MonadPoll m', t m' ~ m) => ConfirmedProposalState -> m ()
    addConfirmedProposal = lift . addConfirmedProposal

    default addActiveProposal
        :: (MonadTrans t, MonadPoll m', t m' ~ m) => ProposalState -> m ()
    addActiveProposal = lift . addActiveProposal

    default deactivateProposal
        :: (MonadTrans t, MonadPoll m', t m' ~ m) => UpId -> m ()
    deactivateProposal = lift . deactivateProposal

instance MonadPoll m => MonadPoll (ReaderT s m)
instance MonadPoll m => MonadPoll (StateT s m)
instance MonadPoll m => MonadPoll (ExceptT s m)
