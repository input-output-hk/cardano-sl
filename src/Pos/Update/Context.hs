module Pos.Update.Context
       ( UpdateContext(..)
       ) where

import           Universum

import           Pos.Update.Poll.Types (ConfirmedProposalState)

data UpdateContext = UpdateContext
    {
    -- | A semaphore which is unlocked when update data is downloaded and
    -- ready to apply.
      ucUpdateSemaphore :: !(MVar ConfirmedProposalState)
    }
