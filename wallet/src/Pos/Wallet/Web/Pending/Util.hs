-- | Pending tx utils which db depends on

module Pos.Wallet.Web.Pending.Util
    ( incPtxSubmitTimingPure
    , mkPtxSubmitTiming
    , ptxMarkAcknowledgedPure
    , resetFailedPtx
    , sortPtxsChrono
    ) where

import           Universum

import           Control.Lens ((*=), (+=), (+~), (<<*=), (<<.=))

import           Pos.Core.Configuration         (HasConfiguration)
import           Pos.Crypto                     (WithHash (..))
import           Pos.Core.Slotting              (FlatSlotId, SlotId, flatSlotId)
import           Pos.Txp                        (TxAux (..), topsortTxs)
import           Pos.Util.Chrono                (OldestFirst (..))
import           Pos.Wallet.Web.Pending.Types   (PendingTx (..), PtxCondition (..),
                                                 PtxSubmitTiming (..), pstNextDelay,
                                                 pstNextSlot, ptxPeerAck, ptxSubmitTiming)

mkPtxSubmitTiming :: HasConfiguration => SlotId -> PtxSubmitTiming
mkPtxSubmitTiming creationSlot =
    PtxSubmitTiming
    { _pstNextSlot  = creationSlot & flatSlotId +~ initialSubmitDelay
    , _pstNextDelay = 1
    }
    where
      initialSubmitDelay = 3 :: FlatSlotId

-- | Sort pending transactions as close as possible to chronological order.
sortPtxsChrono :: [PendingTx] -> OldestFirst [] PendingTx
sortPtxsChrono = OldestFirst . sortWith _ptxCreationSlot . tryTopsort
  where
    tryTopsort txs = fromMaybe txs $ topsortTxs wHash txs
    wHash PendingTx{..} = WithHash (taTx _ptxTxAux) _ptxTxId

incPtxSubmitTimingPure
    :: HasConfiguration
    => PtxSubmitTiming -> PtxSubmitTiming
incPtxSubmitTimingPure = execState $ do
    curDelay <- pstNextDelay <<*= 2
    pstNextSlot . flatSlotId += curDelay

ptxMarkAcknowledgedPure :: PendingTx -> PendingTx
ptxMarkAcknowledgedPure = execState $ do
    wasAcked <- ptxPeerAck <<.= True
    unless wasAcked $ ptxSubmitTiming . pstNextDelay *= 8

-- | If given transaction is in 'PtxWontApply' condition, sets its condition
-- to 'PtxApplying'. This allows "stuck" transactions to be resubmitted
-- again.
--
-- Has no effect for transactions in other conditions.
resetFailedPtx :: HasConfiguration => SlotId -> PendingTx -> PendingTx
resetFailedPtx curSlot ptx@PendingTx{..}
    | PtxWontApply _ poolInfo <- _ptxCond =
          ptx { _ptxCond = PtxApplying poolInfo
              , _ptxSubmitTiming = mkPtxSubmitTiming curSlot
              }
    | otherwise = ptx
