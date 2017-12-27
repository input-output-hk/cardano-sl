{-# LANGUAGE TypeFamilies #-}

-- | Monad abstracting delegation access. Sometimes we want to combine
-- reading from database and some in-memory map, so that's the
-- solution for these cases.

module Pos.Delegation.Cede.Class
       ( MonadCedeRead (..)
       , getPskPk
       , MonadCede (..)
       ) where

import           Universum

import           Control.Monad.Trans (MonadTrans)

import           Pos.Core (ProxySKHeavy, StakeholderId, addressHash)
import           Pos.Crypto (PublicKey)
import           Pos.Delegation.Cede.Types (DlgEdgeAction (..))


-- | This monad abstracts data needed for verifying headers/blocks
-- from the standpoint of delegation component.
class MonadThrow m => MonadCedeRead m where
    -- | Resolve public key of issuer (hashed) to the psk. This method
    -- never returns a revoke psk.
    getPsk :: StakeholderId -> m (Maybe ProxySKHeavy)

    -- | Returns 'True' if given stakeholder has already posted
    -- certificate this epoch.
    hasPostedThisEpoch :: StakeholderId -> m Bool

    -- | Get all stakeholders that have posted cert this epoch.
    getAllPostedThisEpoch :: m (HashSet StakeholderId)

    default getPsk :: (MonadTrans t, MonadCedeRead n, t n ~ m) =>
        StakeholderId -> m (Maybe ProxySKHeavy)
    getPsk = lift . getPsk

    default hasPostedThisEpoch :: (MonadTrans t, MonadCedeRead n, t n ~ m) =>
        StakeholderId -> m Bool
    hasPostedThisEpoch = lift . hasPostedThisEpoch

    default getAllPostedThisEpoch :: (MonadTrans t, MonadCedeRead n, t n ~ m) =>
        m (HashSet StakeholderId)
    getAllPostedThisEpoch = lift getAllPostedThisEpoch


-- | Resolve by public key. Equialent to @getPsk . addressHash@.
getPskPk :: (MonadCedeRead m) => PublicKey -> m (Maybe ProxySKHeavy)
getPskPk = getPsk . addressHash

instance MonadCedeRead m => MonadCedeRead (ReaderT s m)
instance MonadCedeRead m => MonadCedeRead (StateT s m)
instance MonadCedeRead m => MonadCedeRead (ExceptT s m)


-- | 'MonadCedeRead' extension with write capabilities.
class MonadCedeRead m => MonadCede m where
    -- | Modify the content of PSK holder using the edge delegation
    -- action (change/removal).
    modPsk :: DlgEdgeAction -> m ()

    -- | Adds stakeholder to "this epoch posted set".
    addThisEpochPosted :: StakeholderId -> m ()

    -- | Removes stakeholder from "this epoch posted" set.
    delThisEpochPosted :: StakeholderId -> m ()

    default modPsk :: (MonadTrans t, MonadCede n, t n ~ m) => DlgEdgeAction -> m ()
    modPsk = lift . modPsk

    default addThisEpochPosted :: (MonadTrans t, MonadCede n, t n ~ m) => StakeholderId -> m ()
    addThisEpochPosted = lift . addThisEpochPosted

    default delThisEpochPosted :: (MonadTrans t, MonadCede n, t n ~ m) => StakeholderId -> m ()
    delThisEpochPosted = lift . delThisEpochPosted

instance MonadCede m => MonadCede (ReaderT s m)
instance MonadCede m => MonadCede (StateT s m)
instance MonadCede m => MonadCede (ExceptT s m)
