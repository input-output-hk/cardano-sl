-- | Whole in-memory state of UpdateSystem.

module Pos.Update.Context
       ( UpdateContext(..)
       , mkUpdateContext
       ) where

import           Universum

import           Pos.DB.Class              (MonadDBRead)
import           Pos.Slotting              (MonadSlots)
import           Pos.Update.Core           (UpId)
import           Pos.Update.MemState.Types (MemVar, newMemVar)
import           Pos.Update.Poll.Types     (ConfirmedProposalState)

data UpdateContext = UpdateContext
    {
    -- | A semaphore which is unlocked when update data is downloaded and
    -- ready to apply.
      ucUpdateSemaphore    :: !(MVar ConfirmedProposalState)

    -- | Downloading updates by @usUpdate
    , ucDownloadingUpdates :: !(TVar (Set UpId))

    , ucMemState           :: !MemVar
    }

-- | Create initial 'UpdateContext'.
mkUpdateContext ::
       (MonadIO m, MonadDBRead m, MonadSlots m) => m UpdateContext
mkUpdateContext =
    UpdateContext <$> newEmptyMVar <*> newTVarIO mempty <*> newMemVar
