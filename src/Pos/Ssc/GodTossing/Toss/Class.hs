{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Type classes for Toss abstraction.

module Pos.Ssc.GodTossing.Toss.Class
       ( MonadTossRead (..)
       , MonadToss (..)
       ) where

import           Control.Monad.Except    (ExceptT)
import           Control.Monad.Trans     (MonadTrans)
import           System.Wlog             (WithLogger)
import           Universum

import           Pos.Lrc.Types           (RichmenSet)
import           Pos.Ssc.GodTossing.Core (Commitment, Opening, SharesMap,
                                          SignedCommitment, VssCertificate,
                                          VssCertificatesMap)
import           Pos.Types               (EpochIndex, EpochOrSlot, StakeholderId)

----------------------------------------------------------------------------
-- Read-only
----------------------------------------------------------------------------

-- | Type class which provides functions necessary for read-only
-- verification of GodTossing data.
class (Monad m, WithLogger m) =>
      MonadTossRead m where
    -- | Retrieve 'SignedCommitment' of given stakeholder if it's known.
    getCommitment :: StakeholderId -> m (Maybe SignedCommitment)

    -- | Check whether there is an 'Opening' from given stakeholder.
    hasOpening :: StakeholderId -> m Bool

    -- | Check whether there are 'Shares' from given stakeholder.
    hasShares :: StakeholderId -> m Bool

    -- | Check whether there is 'VssCertificate' from given stakeholder.
    hasCertificate :: StakeholderId -> m Bool

    -- | Retrieve all stable 'VssCertificate's for given epoch.
    getStableCertificates :: EpochIndex -> m VssCertificatesMap

    -- | Retrieve richmen for given epoch if they are known.
    getRichmen :: EpochIndex -> m (Maybe RichmenSet)

    -- | Default implementations for 'MonadTrans'.
    default getCommitment :: (MonadTrans t, MonadTossRead m', t m' ~ m) =>
        StakeholderId -> m (Maybe SignedCommitment)
    getCommitment = lift . getCommitment

    default hasOpening :: (MonadTrans t, MonadTossRead m', t m' ~ m) =>
        StakeholderId -> m Bool
    hasOpening = lift . hasOpening

    default hasShares :: (MonadTrans t, MonadTossRead m', t m' ~ m) =>
        StakeholderId -> m Bool
    hasShares = lift . hasShares

    default hasCertificate :: (MonadTrans t, MonadTossRead m', t m' ~ m) =>
        StakeholderId -> m Bool
    hasCertificate = lift . hasCertificate

    default getStableCertificates :: (MonadTrans t, MonadTossRead m', t m' ~ m) =>
        EpochIndex -> m VssCertificatesMap
    getStableCertificates = lift . getStableCertificates

    default getRichmen :: (MonadTrans t, MonadTossRead m', t m' ~ m) =>
        EpochIndex -> m (Maybe RichmenSet)
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
    -- | Put 'Commitment' into state.
    putCommitment :: Commitment -> m ()

    -- | Put 'Opening' from given stakeholder into state.
    putOpening :: StakeholderId -> Opening -> m ()

    -- | Put 'Shares' from given stakeholder into state.
    putShares :: StakeholderId -> SharesMap -> m ()

    -- | Put 'VssCertificate' into state.
    putCertificate :: VssCertificate -> m ()

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
        Commitment -> m ()
    putCommitment = lift . putCommitment

    default putOpening :: (MonadTrans t, MonadToss m', t m' ~ m) =>
        StakeholderId -> Opening -> m ()
    putOpening id = lift . putOpening id

    default putShares :: (MonadTrans t, MonadToss m', t m' ~ m) =>
        StakeholderId -> SharesMap -> m ()
    putShares id = lift . putShares id

    default putCertificate :: (MonadTrans t, MonadToss m', t m' ~ m) =>
        VssCertificate -> m ()
    putCertificate = lift . putCertificate

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
