-- | Pending tx utils which db depends on
-- TODO [CSM-407] merge with Utilshs

module Pos.Wallet.Web.Pending.Updates
    ( mkPtxSubmitTiming
    , incPtxSubmitTimingPure
    , ptxMarkAcknowledgedPure
    , cancelApplyingPtx
    ) where

import           Universum

import           Control.Lens                    ((%=), (+=), (+~), (<<*=), (<<.=))

import           Pos.Core.Configuration.Protocol (HasProtocolConstants)
import           Pos.Core.Slotting               (flatSlotId)
import           Pos.Core.Types                  (FlatSlotId, SlotId)
import           Pos.Wallet.Web.Pending.Types    (PendingTx (..), PtxCondition (..),
                                                  PtxSubmitTiming (..),
                                                  pstNextDelay, pstNextSlot, ptxPeerAck,
                                                  ptxSubmitTiming)


mkPtxSubmitTiming :: HasProtocolConstants => SlotId -> PtxSubmitTiming
mkPtxSubmitTiming creationSlot =
    PtxSubmitTiming
    { _pstNextSlot  = creationSlot & flatSlotId +~ initialSubmitDelay
    , _pstNextDelay = 1
    }
  where
    initialSubmitDelay = 3 :: FlatSlotId

incPtxSubmitTimingPure
    :: HasProtocolConstants
    => PtxSubmitTiming -> PtxSubmitTiming
incPtxSubmitTimingPure = execState $ do
    curDelay <- pstNextDelay <<*= 2
    pstNextSlot . flatSlotId += curDelay

ptxMarkAcknowledgedPure :: PendingTx -> PendingTx
ptxMarkAcknowledgedPure = execState $ do
    wasAcked <- ptxPeerAck <<.= True
    unless wasAcked $ ptxSubmitTiming . pstNextDelay %= (* 8)

-- | If given pending transaction is not yet confirmed, cancels it.
cancelApplyingPtx :: PendingTx -> PendingTx
cancelApplyingPtx ptx@PendingTx{..}
    | PtxApplying poolInfo <- _ptxCond =
          ptx { _ptxCond = PtxWontApply reason poolInfo
              , _ptxPeerAck = False
              }
    | otherwise = ptx
  where
    reason = "Canceled manually"
