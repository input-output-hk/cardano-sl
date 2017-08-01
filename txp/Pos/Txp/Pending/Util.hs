-- | Pending transactions processing.

module Pos.Txp.Pending.Util
    ( ptxCreationSlot
    , ptxExpireSlot

    , isPtxInBlocks
    ) where

import           Universum

import           Pos.Core              (SlotId)
import           Pos.Txp.Pending.Types (PendingTx, PtxCondition (..))

ptxCreationSlot :: PendingTx -> SlotId
ptxCreationSlot = undefined

ptxExpireSlot :: PendingTx -> SlotId
ptxExpireSlot = undefined

isPtxInBlocks :: PtxCondition -> Bool
isPtxInBlocks = \case
    PtxApplying      -> False
    PtxInUpperBlocks -> True
    PtxPersisted     -> True
    PtxWon'tApply _  -> False
