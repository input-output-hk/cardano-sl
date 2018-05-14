-- | Whole in-memory state of UpdateSystem.

module Pos.Update.Context
       ( UpdateContext(..)
       , mkUpdateContext
       ) where

import           Universum

import           Pos.Core (HasProtocolConstants)
import           Pos.DB.Class (MonadDBRead)
import           Pos.Slotting (MonadSlots)
import           Pos.Update.MemState.Types (MemVar, newMemVar)
import           Pos.Update.Poll.Types (ConfirmedProposalState)

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
    :: forall ctx m.
    ( HasProtocolConstants
    , MonadIO m
    , MonadDBRead m
    , MonadSlots ctx m
    )
    => m UpdateContext
mkUpdateContext = UpdateContext <$> newEmptyMVar <*> newMVar () <*> newMemVar
