{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Type class necessary for Update System.

module Pos.Update.Class
       ( MonadUS (..)
       ) where

import           Control.Concurrent.STM (TVar)
import           Control.Monad.Trans    (MonadTrans)
import           Universum


import           Pos.DHT.Model.Class    (DHTResponseT)
import           Pos.DHT.Real           (KademliaDHT)
import           Pos.Script.Type        (ScriptVersion)
import           Pos.Types              (ApplicationName, ProtocolVersion)
import           Pos.Update.MemState    (LocalProposalState, MemState)
import           Pos.Update.Types       (UpId)

-- | Equivalent of @MonadReader (TVar MemState) m@.
-- TODO: askUSMemState and all the other things should probably be separated
class Monad m => MonadUS m where
    askUSMemState :: m (TVar MemState)
    -- ^ Retrieve 'TVar' on 'Pos.Update.State.MemState'.
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
    default askUSMemState
        :: (MonadTrans t, MonadUS m', t m' ~ m) => m (TVar MemState)
    askUSMemState = lift askUSMemState

    default getScriptVersion
        :: (MonadTrans t, MonadUS m', t m' ~ m) => ProtocolVersion -> m (Maybe ScriptVersion)
    getScriptVersion = lift . getScriptVersion

    default getLastAdoptedPV
        :: (MonadTrans t, MonadUS m', t m' ~ m) => m ProtocolVersion
    getLastAdoptedPV = lift getLastAdoptedPV

    default getLastConfirmedSV
        :: (MonadTrans t, MonadUS m', t m' ~ m) => ApplicationName -> m (Maybe Word32)
    getLastConfirmedSV = lift . getLastConfirmedSV

    default hasActiveProposal
        :: (MonadTrans t, MonadUS m', t m' ~ m) => ApplicationName -> m Bool
    hasActiveProposal = lift . hasActiveProposal

    default getProposal
        :: (MonadTrans t, MonadUS m', t m' ~ m) => UpId -> m LocalProposalState
    getProposal = lift . getProposal

instance MonadUS m => MonadUS (ReaderT s m)
instance MonadUS m => MonadUS (StateT s m)
instance MonadUS m => MonadUS (DHTResponseT s m)
instance MonadUS m => MonadUS (KademliaDHT m)
