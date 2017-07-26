-- | Pending transactions processing.

module Pos.Txp.Pending.Util
    ( ptxCreationSlot
    , ptxExpireSlot
    ) where

import           Universum

import           Pos.Core              (SlotId)
import           Pos.Txp.Pending.Types (PendingTx)

ptxCreationSlot :: PendingTx -> SlotId
ptxCreationSlot = undefined

ptxExpireSlot :: PendingTx -> SlotId
ptxExpireSlot = undefined

