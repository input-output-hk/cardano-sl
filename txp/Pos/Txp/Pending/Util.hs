-- | Pending transactions processing.

module Pos.Txp.Pending.Util
    ( isPtxInBlocks
    ) where

import           Universum

import           Pos.Txp.Pending.Types (PtxCondition (..))


isPtxInBlocks :: PtxCondition -> Bool
isPtxInBlocks = \case
    PtxApplying{}      -> False
    PtxInUpperBlocks{} -> True
    PtxPersisted{}     -> True
    PtxWon'tApply{}    -> False
