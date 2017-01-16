{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Type class for Poll abstraction.

module Pos.Update.Poll.Class
       ( MonadPollRead (..)
       ) where

import           Control.Monad.Except (ExceptT)
import           Control.Monad.Trans  (MonadTrans)
import           Universum

import           Pos.DB.Types         (ProposalState)
import           Pos.Script.Type      (ScriptVersion)
import           Pos.Types            (ApplicationName, NumSoftwareVersion,
                                       ProtocolVersion)
import           Pos.Update.Core      (UpId)

-- | Type class which provides function necessary for verification of US data.
class Monad m => MonadPollRead m where
    getScriptVersion :: ProtocolVersion -> m (Maybe ScriptVersion)
    -- ^ Retrieve script version for given protocol version
    getLastAdoptedPV :: m ProtocolVersion
    -- ^ Get last protocol version
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
