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

import           Control.Monad.Trans       (MonadTrans)

import           Pos.Crypto                (PublicKey)
import           Pos.Delegation.Cede.Types (DlgEdgeAction (..))
import           Pos.Types                 (ProxySKHeavy, StakeholderId, addressHash)


-- | This monad includes psk reading capabilities.
class MonadThrow m => MonadCedeRead m where
    -- | Resolve public key of issuer (hashed) to the psk. This method
    -- never returns a revoke psk.
    getPsk :: StakeholderId -> m (Maybe ProxySKHeavy)

    default getPsk :: (MonadTrans t, MonadCedeRead n, t n ~ m) =>
        StakeholderId -> m (Maybe ProxySKHeavy)
    getPsk = lift . getPsk

-- | Resolve by public key. Equialent to @getPsk . addressHash@.
getPskPk :: (MonadCedeRead m) => PublicKey -> m (Maybe ProxySKHeavy)
getPskPk = getPsk . addressHash

instance MonadCedeRead m => MonadCedeRead (ReaderT s m)
instance MonadCedeRead m => MonadCedeRead (StateT s m)
instance MonadCedeRead m => MonadCedeRead (ExceptT s m)


-- | Monad that can modify PSK contents inside.
class MonadCedeRead m => MonadCede m where
    -- | Modify the content of PSK holder using the edge delegation
    -- action (change/removal).
    modPsk :: DlgEdgeAction -> m ()

    default modPsk :: (MonadTrans t, MonadCede n, t n ~ m) =>
        DlgEdgeAction -> m ()
    modPsk = lift . modPsk

instance MonadCede m => MonadCede (ReaderT s m)
instance MonadCede m => MonadCede (StateT s m)
instance MonadCede m => MonadCede (ExceptT s m)
