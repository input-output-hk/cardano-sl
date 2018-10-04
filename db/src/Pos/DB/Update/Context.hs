-- | Whole in-memory state of UpdateSystem.

module Pos.DB.Update.Context
       ( UpdateContext(..)
       , mkUpdateContext
       ) where

import           Universum

import           Pos.Chain.Update (ConfirmedProposalState)
import           Pos.Core.Slotting (MonadSlots, SlotCount)
import           Pos.DB.Class (MonadDBRead)
import           Pos.DB.Update.MemState.Types (MemVar, newMemVar)

data UpdateContext = UpdateContext
    {
    -- | A semaphore which is unlocked when update data is downloaded and
    -- ready to apply.
      ucDownloadedUpdate :: !(MVar ConfirmedProposalState)

    -- | A lock which allows only one thread to download an update.
    , ucDownloadLock     :: !(MVar ())

    -- | In-memory state of update-system-as-block-component.
    , ucMemState         :: !MemVar
    }

-- | Create initial 'UpdateContext'.
mkUpdateContext
    :: forall ctx m
     . (MonadIO m, MonadDBRead m, MonadSlots ctx m)
    => SlotCount
    -> m UpdateContext
mkUpdateContext epochSlots =
    UpdateContext <$> newEmptyMVar <*> newMVar () <*> newMemVar epochSlots
