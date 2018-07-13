-- | Types related to Toss.

module Pos.Ssc.Toss.Types
       ( isGoodSlotForTag
       , isGoodSlotIdForTag

       , module Sinbin
       ) where

import           Universum

import           Pos.Core (HasProtocolConstants, LocalSlotIndex, SlotId)
import           Pos.Sinbin.Ssc.Toss.Types as Sinbin
import           Pos.Ssc.Base (isCommitmentId, isCommitmentIdx, isOpeningId,
                     isOpeningIdx, isSharesId, isSharesIdx)


isGoodSlotForTag :: HasProtocolConstants => SscTag -> LocalSlotIndex -> Bool
isGoodSlotForTag CommitmentMsg     = isCommitmentIdx
isGoodSlotForTag OpeningMsg        = isOpeningIdx
isGoodSlotForTag SharesMsg         = isSharesIdx
isGoodSlotForTag VssCertificateMsg = const True

isGoodSlotIdForTag :: HasProtocolConstants => SscTag -> SlotId -> Bool
isGoodSlotIdForTag CommitmentMsg     = isCommitmentId
isGoodSlotIdForTag OpeningMsg        = isOpeningId
isGoodSlotIdForTag SharesMsg         = isSharesId
isGoodSlotIdForTag VssCertificateMsg = const True
