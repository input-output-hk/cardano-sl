-- | Update system-specific functionality related to 'BlockVersion',
-- 'BlockVersionData', 'BlockVersionModifier'.

module Pos.Update.Core.BlockVersion
       ( applyBVM
       ) where

import           Universum

import           Pos.Core              (BlockVersionData (..))
import           Pos.Update.Core.Types (BlockVersionModifier (..))

-- | Apply 'BlockVersionModifier' to 'BlockVersionData'.
applyBVM :: BlockVersionModifier -> BlockVersionData -> BlockVersionData
applyBVM BlockVersionModifier {..} BlockVersionData {..} =
    BlockVersionData
    { bvdScriptVersion     =                        bvmScriptVersion
    , bvdSlotDuration      =                         bvmSlotDuration
    , bvdMaxBlockSize      =                         bvmMaxBlockSize
    , bvdMaxHeaderSize     =                        bvmMaxHeaderSize
    , bvdMaxTxSize         =                            bvmMaxTxSize
    , bvdMaxProposalSize   =                      bvmMaxProposalSize
    , bvdMpcThd            =                               bvmMpcThd
    , bvdHeavyDelThd       =                          bvmHeavyDelThd
    , bvdUpdateVoteThd     =                        bvmUpdateVoteThd
    , bvdUpdateProposalThd =                    bvmUpdateProposalThd
    , bvdUpdateImplicit    =                       bvmUpdateImplicit
    , bvdUpdateSoftforkThd =                    bvmUpdateSoftforkThd
    , bvdTxFeePolicy       = fromMaybe bvdTxFeePolicy bvmTxFeePolicy
    }
