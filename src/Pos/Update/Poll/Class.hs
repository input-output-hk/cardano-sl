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

import           Pos.Script.Type       (ScriptVersion)
import           Pos.Types             (ApplicationName, ChainDifficulty, Coin,
                                        EpochIndex, NumSoftwareVersion, ProtocolVersion,
                                        SlotId, SoftwareVersion, StakeholderId)
import           Pos.Update.Core       (UpId)
import           Pos.Update.Poll.Types (DecidedProposalState, ProposalState,
                                        UndecidedProposalState)

----------------------------------------------------------------------------
-- Read-only
----------------------------------------------------------------------------

-- | Type class which provides function necessary for read-only
-- verification of US data.
class Monad m => MonadPollRead m where
    getScriptVersion :: ProtocolVersion -> m (Maybe ScriptVersion)
    -- ^ Retrieve script version for given protocol version
    getLastAdoptedPV :: m ProtocolVersion
    -- ^ Get last adopted protocol version
    getLastConfirmedSV :: ApplicationName -> m (Maybe NumSoftwareVersion)
    -- ^ Get number of last confirmed version of application
    hasActiveProposal :: ApplicationName -> m Bool
    -- ^ Check if given application has an active (non-confirmed) proposal
    getProposal :: UpId -> m (Maybe ProposalState)
    -- ^ Get active proposal
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

    -- | Default implementations for 'MonadTrans'.
    default getScriptVersion
        :: (MonadTrans t, MonadPollRead m', t m' ~ m) => ProtocolVersion -> m (Maybe ScriptVersion)
    getScriptVersion = lift . getScriptVersion

    default getLastAdoptedPV
        :: (MonadTrans t, MonadPollRead m', t m' ~ m) => m ProtocolVersion
    getLastAdoptedPV = lift getLastAdoptedPV

    default getLastConfirmedSV
        :: (MonadTrans t, MonadPollRead m', t m' ~ m) => ApplicationName -> m (Maybe Word32)
    getLastConfirmedSV = lift . getLastConfirmedSV

    default hasActiveProposal
        :: (MonadTrans t, MonadPollRead m', t m' ~ m) => ApplicationName -> m Bool
    hasActiveProposal = lift . hasActiveProposal

    default getProposal
        :: (MonadTrans t, MonadPollRead m', t m' ~ m) =>
        UpId -> m (Maybe ProposalState)
    getProposal = lift . getProposal

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

instance MonadPollRead m => MonadPollRead (ReaderT s m)
instance MonadPollRead m => MonadPollRead (StateT s m)
instance MonadPollRead m => MonadPollRead (ExceptT s m)

----------------------------------------------------------------------------
-- Writeable
----------------------------------------------------------------------------

-- | Type class which provides function necessary for verification of
-- US data with ability to modify state.
class MonadPollRead m => MonadPoll m where
    addScriptVersionDep :: ProtocolVersion -> ScriptVersion -> m ()
    -- ^ Add functional dependency between protocol version and script version.
    delScriptVersionDep :: ProtocolVersion -> m ()
    -- ^ Delete functional dependency between protocol version and script version.
    setLastAdoptedPV :: ProtocolVersion -> m ()
    -- ^ Set last adopted protocol version.
    setLastConfirmedSV :: SoftwareVersion -> m ()
    -- ^ Set last confirmed version of application.
    delConfirmedSV :: ApplicationName -> m ()
    -- ^ Del last confirmed version of application.
    addActiveProposal :: ProposalState -> m ()
    -- ^ Add new active proposal with its state.
    deactivateProposal :: UpId -> m ()
    -- ^ Delete active proposal given its name and identifier.

    -- | Default implementations for 'MonadTrans'.
    default addScriptVersionDep
        :: (MonadTrans t, MonadPoll m', t m' ~ m) => ProtocolVersion -> ScriptVersion -> m ()
    addScriptVersionDep pv = lift . addScriptVersionDep pv

    default delScriptVersionDep
        :: (MonadTrans t, MonadPoll m', t m' ~ m) => ProtocolVersion -> m ()
    delScriptVersionDep = lift . delScriptVersionDep

    default setLastAdoptedPV
        :: (MonadTrans t, MonadPoll m', t m' ~ m) => ProtocolVersion -> m ()
    setLastAdoptedPV = lift . setLastAdoptedPV

    default setLastConfirmedSV
        :: (MonadTrans t, MonadPoll m', t m' ~ m) => SoftwareVersion -> m ()
    setLastConfirmedSV = lift . setLastConfirmedSV

    default delConfirmedSV
        :: (MonadTrans t, MonadPoll m', t m' ~ m) => ApplicationName -> m ()
    delConfirmedSV = lift . delConfirmedSV

    default addActiveProposal
        :: (MonadTrans t, MonadPoll m', t m' ~ m) => ProposalState -> m ()
    addActiveProposal = lift . addActiveProposal

    default deactivateProposal
        :: (MonadTrans t, MonadPoll m', t m' ~ m) => UpId -> m ()
    deactivateProposal = lift . deactivateProposal

instance MonadPoll m => MonadPoll (ReaderT s m)
instance MonadPoll m => MonadPoll (StateT s m)
instance MonadPoll m => MonadPoll (ExceptT s m)
