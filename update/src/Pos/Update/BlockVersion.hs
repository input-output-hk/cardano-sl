-- | Update system-specific functionality related to 'BlockVersion',
-- 'BlockVersionData', 'BlockVersionModifier'.

module Pos.Update.BlockVersion
       ( applyBVM
       ) where

import           Universum

import           Pos.Core (BlockVersionData (..))
import           Pos.Core.Update (BlockVersionModifier (..))

-- | Apply 'BlockVersionModifier' to 'BlockVersionData'.
applyBVM :: BlockVersionModifier -> BlockVersionData -> BlockVersionData
applyBVM BlockVersionModifier {..} BlockVersionData {..} =
    BlockVersionData
    { bvdScriptVersion     = fromMaybe bvdScriptVersion     bvmScriptVersion
    , bvdSlotDuration      = fromMaybe bvdSlotDuration      bvmSlotDuration
    , bvdMaxBlockSize      = fromMaybe bvdMaxBlockSize      bvmMaxBlockSize
    , bvdMaxHeaderSize     = fromMaybe bvdMaxHeaderSize     bvmMaxHeaderSize
    , bvdMaxTxSize         = fromMaybe bvdMaxTxSize         bvmMaxTxSize
    , bvdMaxProposalSize   = fromMaybe bvdMaxProposalSize   bvmMaxProposalSize
    , bvdMpcThd            = fromMaybe bvdMpcThd            bvmMpcThd
    , bvdHeavyDelThd       = fromMaybe bvdHeavyDelThd       bvmHeavyDelThd
    , bvdUpdateVoteThd     = fromMaybe bvdUpdateVoteThd     bvmUpdateVoteThd
    , bvdUpdateProposalThd = fromMaybe bvdUpdateProposalThd bvmUpdateProposalThd
    , bvdUpdateImplicit    = fromMaybe bvdUpdateImplicit    bvmUpdateImplicit
    , bvdSoftforkRule      = fromMaybe bvdSoftforkRule      bvmSoftforkRule
    , bvdTxFeePolicy       = fromMaybe bvdTxFeePolicy       bvmTxFeePolicy
    , bvdUnlockStakeEpoch  = fromMaybe bvdUnlockStakeEpoch  bvmUnlockStakeEpoch
    }
