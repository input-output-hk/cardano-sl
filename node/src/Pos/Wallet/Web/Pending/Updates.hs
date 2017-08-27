-- | Pending tx utils which db depends on

module Pos.Wallet.Web.Pending.Updates
    ( incPtxSubmitTimingPure
    , ptxMarkAcknowledgedPure
    ) where

import           Universum

import           Control.Lens                 ((%=), (+=), (<<*=), (<<.=))

import           Pos.Core.Context             (HasCoreConstants)
import           Pos.Core.Slotting            (flatSlotId)
import           Pos.Wallet.Web.Pending.Types (PendingTx (..), PtxSubmitTiming (..),
                                               pstNextDelay, pstNextSlot, ptxPeerAck,
                                               ptxSubmitTiming)


incPtxSubmitTimingPure
    :: HasCoreConstants
    => PtxSubmitTiming -> PtxSubmitTiming
incPtxSubmitTimingPure = execState $ do
    curDelay <- pstNextDelay <<*= 2
    pstNextSlot . flatSlotId += curDelay

ptxMarkAcknowledgedPure :: PendingTx -> PendingTx
ptxMarkAcknowledgedPure = execState $ do
    wasAcked <- ptxPeerAck <<.= True
    unless wasAcked $ ptxSubmitTiming . pstNextDelay %= (* 8)

