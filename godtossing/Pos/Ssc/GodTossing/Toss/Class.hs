{-# LANGUAGE TypeFamilies #-}

-- | Type classes for Toss abstraction.

module Pos.Ssc.GodTossing.Toss.Class
       ( MonadTossRead (..)
       , MonadToss (..)
       ) where

import           Control.Monad.Except           (ExceptT)
import           Control.Monad.Trans            (MonadTrans)
import           System.Wlog                    (WithLogger)
import           Universum

import           Pos.Core                       (EpochIndex, EpochOrSlot, StakeholderId)
import           Pos.Lrc.Types                  (RichmenStake)
import           Pos.Ssc.GodTossing.Core        (CommitmentsMap, InnerSharesMap, Opening,
                                                 OpeningsMap, SharesMap, SignedCommitment,
                                                 VssCertificate, VssCertificatesMap)
import           Pos.Ssc.GodTossing.VssCertData (VssCertData)

----------------------------------------------------------------------------
-- Read-only
----------------------------------------------------------------------------

-- | Type class which provides functions necessary for read-only
-- verification of GodTossing data.
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

    -- | Get 'VssCertData'.
    getVssCertData :: m VssCertData

    -- | Retrieve all stable 'VssCertificate's for given epoch.
    getStableCertificates :: EpochIndex -> m VssCertificatesMap

    -- | Retrieve richmen for given epoch if they are known.
    getRichmen :: EpochIndex -> m (Maybe RichmenStake)

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

    default getVssCertData :: (MonadTrans t, MonadTossRead m', t m' ~ m) =>
        m VssCertData
    getVssCertData = lift getVssCertData

    default getVssCertificates :: (MonadTrans t, MonadTossRead m', t m' ~ m) =>
        m VssCertificatesMap
    getVssCertificates = lift getVssCertificates

    default getStableCertificates :: (MonadTrans t, MonadTossRead m', t m' ~ m) =>
        EpochIndex -> m VssCertificatesMap
    getStableCertificates = lift . getStableCertificates

    default getRichmen :: (MonadTrans t, MonadTossRead m', t m' ~ m) =>
        EpochIndex -> m (Maybe RichmenStake)
    getRichmen = lift . getRichmen

instance MonadTossRead m => MonadTossRead (ReaderT s m)
instance MonadTossRead m => MonadTossRead (StateT s m)
instance MonadTossRead m => MonadTossRead (ExceptT s m)

----------------------------------------------------------------------------
-- Writeable
----------------------------------------------------------------------------

-- | Type class which provides function necessary for verification of
-- GodTossing data with ability to modify state.
class MonadTossRead m =>
      MonadToss m where
    -- | Put 'SignedCommitment' into state.
    putCommitment :: SignedCommitment -> m ()

    -- | Put 'Opening' from given stakeholder into state.
    putOpening :: StakeholderId -> Opening  -> m ()

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
