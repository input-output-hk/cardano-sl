{-# LANGUAGE TypeFamilies #-}

-- | Type classes for Toss abstraction.

module Pos.Ssc.Toss.Class
       ( MonadTossRead (..)
       , MonadTossEnv (..)
       , MonadToss (..)
       ) where

import           Universum hiding (id)

import           Control.Monad.Except (ExceptT)
import           Control.Monad.Trans (MonadTrans)
import           System.Wlog (WithLogger)

import           Pos.Core (BlockVersionData, EpochIndex, EpochOrSlot, StakeholderId, VssCertificate,
                           VssCertificatesMap)
import           Pos.Core.Ssc (CommitmentsMap, InnerSharesMap, Opening, OpeningsMap, SharesMap,
                               SignedCommitment)
import           Pos.Lrc.Types (RichmenStakes)

----------------------------------------------------------------------------
-- Read-only
----------------------------------------------------------------------------

-- | Type class which provides functions necessary for read-only
-- verification of SSC data.
class (Monad m, WithLogger m) =>
      MonadTossRead m where
    -- | Get 'CommitmentsMap' with all commitments.
    getCommitments :: m CommitmentsMap

    -- | Get 'OpeningsMap' with all openings.
    getOpenings :: m OpeningsMap

    -- | Get 'SharesMap' with all shares.
    getShares :: m SharesMap

    -- | Get 'VssCertificatesMap' with all VSS certificates.
    getVssCertificates :: m VssCertificatesMap

    -- | Retrieve all stable 'VssCertificate's for given epoch.
    getStableCertificates :: EpochIndex -> m VssCertificatesMap

    -- | Default implementations for 'MonadTrans'.
    default getCommitments :: (MonadTrans t, MonadTossRead m', t m' ~ m) =>
        m CommitmentsMap
    getCommitments = lift getCommitments

    default getOpenings :: (MonadTrans t, MonadTossRead m', t m' ~ m) =>
        m OpeningsMap
    getOpenings = lift getOpenings

    default getShares :: (MonadTrans t, MonadTossRead m', t m' ~ m) =>
        m SharesMap
    getShares = lift getShares

    default getVssCertificates :: (MonadTrans t, MonadTossRead m', t m' ~ m) =>
        m VssCertificatesMap
    getVssCertificates = lift getVssCertificates

    default getStableCertificates :: (MonadTrans t, MonadTossRead m', t m' ~ m) =>
        EpochIndex -> m VssCertificatesMap
    getStableCertificates = lift . getStableCertificates

instance MonadTossRead m => MonadTossRead (ReaderT s m)
instance MonadTossRead m => MonadTossRead (StateT s m)
instance MonadTossRead m => MonadTossRead (ExceptT s m)

----------------------------------------------------------------------------
-- Environment
----------------------------------------------------------------------------

class Monad m => MonadTossEnv m where
    -- | Retrieve richmen for given epoch if they are known.
    getRichmen :: EpochIndex -> m (Maybe RichmenStakes)

    -- | Retrieve current adopted block data
    getAdoptedBVData :: m BlockVersionData

    default getRichmen :: (MonadTrans t, MonadTossEnv m', t m' ~ m) =>
        EpochIndex -> m (Maybe RichmenStakes)
    getRichmen = lift . getRichmen

    default getAdoptedBVData :: (MonadTrans t, MonadTossEnv m', t m' ~ m) =>
        m BlockVersionData
    getAdoptedBVData = lift getAdoptedBVData

instance MonadTossEnv m => MonadTossEnv (ReaderT s m)
instance MonadTossEnv m => MonadTossEnv (StateT s m)
instance MonadTossEnv m => MonadTossEnv (ExceptT s m)

----------------------------------------------------------------------------
-- Writeable
----------------------------------------------------------------------------

-- | Type class which provides function necessary for verification of
-- SSC data with ability to modify state.
class MonadTossRead m =>
      MonadToss m where
    -- | Put 'SignedCommitment' into state.
    putCommitment :: SignedCommitment -> m ()

    -- | Put 'Opening' from given stakeholder into state.
    putOpening :: StakeholderId -> Opening -> m ()

    -- | Put 'InnerShares' from given stakeholder into state.
    putShares :: StakeholderId -> InnerSharesMap -> m ()

    -- | Put 'VssCertificate' into state.
    putCertificate :: VssCertificate -> m ()

    -- | Reset Commitments and Openings.
    resetCO :: m ()

    -- | Reset Shares.
    resetShares :: m ()

    -- | Delete commitment of given stakeholder.
    delCommitment :: StakeholderId -> m ()

    -- | Delete opening of given stakeholder.
    delOpening :: StakeholderId -> m ()

    -- | Delete shares of given stakeholder.
    delShares :: StakeholderId -> m ()

    -- | This function is called when block with given 'EpochOrSlot' is applied.
    setEpochOrSlot :: EpochOrSlot -> m ()

    -- | Default implementations for 'MonadTrans'.
    default putCommitment :: (MonadTrans t, MonadToss m', t m' ~ m) =>
        SignedCommitment -> m ()
    putCommitment = lift . putCommitment

    default putOpening :: (MonadTrans t, MonadToss m', t m' ~ m) =>
        StakeholderId -> Opening -> m ()
    putOpening id = lift . putOpening id

    default putShares :: (MonadTrans t, MonadToss m', t m' ~ m) =>
        StakeholderId -> InnerSharesMap -> m ()
    putShares id = lift . putShares id

    default putCertificate :: (MonadTrans t, MonadToss m', t m' ~ m) =>
        VssCertificate -> m ()
    putCertificate = lift . putCertificate

    default resetCO :: (MonadTrans t, MonadToss m', t m' ~ m) =>
        m ()
    resetCO = lift resetCO

    default resetShares :: (MonadTrans t, MonadToss m', t m' ~ m) =>
        m ()
    resetShares = lift resetShares

    default delCommitment :: (MonadTrans t, MonadToss m', t m' ~ m) =>
        StakeholderId -> m ()
    delCommitment = lift . delCommitment

    default delOpening :: (MonadTrans t, MonadToss m', t m' ~ m) =>
        StakeholderId -> m ()
    delOpening = lift . delOpening

    default delShares :: (MonadTrans t, MonadToss m', t m' ~ m) =>
        StakeholderId -> m ()
    delShares = lift . delShares

    default setEpochOrSlot :: (MonadTrans t, MonadToss m', t m' ~ m) =>
        EpochOrSlot -> m ()
    setEpochOrSlot = lift . setEpochOrSlot

instance MonadToss m => MonadToss (ReaderT s m)
instance MonadToss m => MonadToss (StateT s m)
instance MonadToss m => MonadToss (ExceptT s m)
