-- | Pending tx utils which db depends on

module Pos.Wallet.Web.Pending.Updates
    ( incPtxSubmitTimingPure
    , ptxMarkAcknowledgedPure
    ) where

import           Universum

import           Control.Lens                 ((%=), (<<.=))

import           Pos.Core.Context             (HasCoreConstants)
import           Pos.Core.Slotting            (flattenSlotId, unflattenSlotId)
import           Pos.Wallet.Web.Pending.Types (PendingTx (..), PtxSubmitTiming (..),
                                               pstNextDelay, ptxPeerAck, ptxSubmitTiming)


incPtxSubmitTimingPure :: HasCoreConstants => PtxSubmitTiming -> PtxSubmitTiming
incPtxSubmitTimingPure PtxSubmitTiming{..} =
    PtxSubmitTiming
    { _pstNextSlot = unflattenSlotId $ flattenSlotId _pstNextSlot + _pstNextDelay
    , _pstNextDelay = 2 * _pstNextDelay
    }

ptxMarkAcknowledgedPure :: PendingTx -> PendingTx
ptxMarkAcknowledgedPure = execState $ do
    wasAcked <- ptxPeerAck <<.= True
    unless wasAcked $ ptxSubmitTiming . pstNextDelay %= (* 8)

