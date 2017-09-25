-- | Pending tx utils which db depends on
-- TODO [CSM-407] merge with Utilshs

module Pos.Wallet.Web.Pending.Updates
    ( mkPtxSubmitTiming
    , incPtxSubmitTimingPure
    , ptxMarkAcknowledgedPure
    ) where

import           Universum

import           Control.Lens                 ((%=), (+=), (+~), (<<*=), (<<.=))

import           Pos.Core.Configuration       (HasConfiguration)
import           Pos.Core.Slotting            (flatSlotId)
import           Pos.Core.Types               (FlatSlotId, SlotId)
import           Pos.Wallet.Web.Pending.Types (PendingTx (..), PtxSubmitTiming (..),
                                               pstNextDelay, pstNextSlot, ptxPeerAck,
                                               ptxSubmitTiming)


mkPtxSubmitTiming :: HasConfiguration => SlotId -> PtxSubmitTiming
mkPtxSubmitTiming creationSlot =
    PtxSubmitTiming
    { _pstNextSlot  = creationSlot & flatSlotId +~ initialSubmitDelay
    , _pstNextDelay = 1
    }
  where
    initialSubmitDelay = 3 :: FlatSlotId

incPtxSubmitTimingPure
    :: HasConfiguration
    => PtxSubmitTiming -> PtxSubmitTiming
incPtxSubmitTimingPure = execState $ do
    curDelay <- pstNextDelay <<*= 2
    pstNextSlot . flatSlotId += curDelay

ptxMarkAcknowledgedPure :: PendingTx -> PendingTx
ptxMarkAcknowledgedPure = execState $ do
    wasAcked <- ptxPeerAck <<.= True
    unless wasAcked $ ptxSubmitTiming . pstNextDelay %= (* 8)
