module Pos.Update.Context
       ( UpdateContext(..)
       ) where

import           Universum

import           Pos.Update.Core           (UpId)
import           Pos.Update.MemState.Types (MemVar)
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
