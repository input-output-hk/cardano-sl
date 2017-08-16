-- | Pending transactions processing.

module Pos.Wallet.Web.Pending.Util
    ( isPtxInBlocks
    ) where

import           Universum

import           Pos.Wallet.Web.Pending.Types (PtxCondition (..))


isPtxInBlocks :: PtxCondition -> Bool
isPtxInBlocks = \case
    PtxApplying{}      -> False
    PtxInUpperBlocks{} -> True
    PtxPersisted{}     -> True
    PtxWon'tApply{}    -> False
