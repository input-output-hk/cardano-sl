{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Type class for Poll abstraction.

module Pos.Update.Poll.Class
       ( MonadPollRead (..)
       , MonadPoll (..)
       ) where

import           Control.Monad.Except (ExceptT)
import           Control.Monad.Trans  (MonadTrans)
import           Universum

import           Pos.DB.Types         (ProposalState)
import           Pos.Script.Type      (ScriptVersion)
import           Pos.Types            (ApplicationName, NumSoftwareVersion,
                                       ProtocolVersion, SoftwareVersion)
import           Pos.Update.Core      (UpId)

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
    setLastAdoptedPV :: ProtocolVersion -> m ()
    -- ^ Set last adopted protocol version.
    setLastConfirmedSV :: SoftwareVersion -> m ()
    -- ^ Set last confirmed version of application.
    addActiveProposal :: ProposalState -> m ()
    -- ^ Add new active proposal with its state.

    -- | Default implementations for 'MonadTrans'.
    default addScriptVersionDep
        :: (MonadTrans t, MonadPoll m', t m' ~ m) => ProtocolVersion -> ScriptVersion -> m ()
    addScriptVersionDep pv = lift . addScriptVersionDep pv

    default setLastAdoptedPV
        :: (MonadTrans t, MonadPoll m', t m' ~ m) => ProtocolVersion -> m ()
    setLastAdoptedPV = lift . setLastAdoptedPV

    default setLastConfirmedSV
        :: (MonadTrans t, MonadPoll m', t m' ~ m) => SoftwareVersion -> m ()
    setLastConfirmedSV = lift . setLastConfirmedSV

    default addActiveProposal
        :: (MonadTrans t, MonadPoll m', t m' ~ m) => ProposalState -> m ()
    addActiveProposal = lift . addActiveProposal

instance MonadPoll m => MonadPoll (ReaderT s m)
instance MonadPoll m => MonadPoll (StateT s m)
instance MonadPoll m => MonadPoll (ExceptT s m)
