{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Type class for Poll abstraction.

module Pos.Update.Poll.Class
       ( MonadPoll (..)
       ) where

import           Control.Monad.Except      (ExceptT)
import           Control.Monad.Trans       (MonadTrans)
import           Universum

import           Pos.Script.Type           (ScriptVersion)
import           Pos.Types                 (ApplicationName, ProtocolVersion)
import           Pos.Update.MemState.Types (LocalProposalState)
import           Pos.Update.Types          (UpId)

-- | Type class which provides function necessary for verification of US data.
class Monad m => MonadPoll m where
    getScriptVersion :: ProtocolVersion -> m (Maybe ScriptVersion)
    -- ^ Retrieve script version for given protocol version
    getLastAdoptedPV :: m ProtocolVersion
    -- ^ Get last protocol version
    getLastConfirmedSV :: ApplicationName -> m (Maybe Word32)
    -- ^ Get number of last confirmed version of application
    hasActiveProposal :: ApplicationName -> m Bool
    -- ^ Check if given application has an active (non-confirmed) proposal
    getProposal :: UpId -> m LocalProposalState
    -- ^ Get active proposal

    -- | Default implementations for 'MonadTrans'.
    default getScriptVersion
        :: (MonadTrans t, MonadPoll m', t m' ~ m) => ProtocolVersion -> m (Maybe ScriptVersion)
    getScriptVersion = lift . getScriptVersion

    default getLastAdoptedPV
        :: (MonadTrans t, MonadPoll m', t m' ~ m) => m ProtocolVersion
    getLastAdoptedPV = lift getLastAdoptedPV

    default getLastConfirmedSV
        :: (MonadTrans t, MonadPoll m', t m' ~ m) => ApplicationName -> m (Maybe Word32)
    getLastConfirmedSV = lift . getLastConfirmedSV

    default hasActiveProposal
        :: (MonadTrans t, MonadPoll m', t m' ~ m) => ApplicationName -> m Bool
    hasActiveProposal = lift . hasActiveProposal

    default getProposal
        :: (MonadTrans t, MonadPoll m', t m' ~ m) => UpId -> m LocalProposalState
    getProposal = lift . getProposal

instance MonadPoll m => MonadPoll (ReaderT s m)
instance MonadPoll m => MonadPoll (StateT s m)
instance MonadPoll m => MonadPoll (ExceptT s m)
