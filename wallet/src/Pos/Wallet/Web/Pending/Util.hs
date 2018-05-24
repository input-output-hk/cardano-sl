-- | Pending tx utils which db depends on

module Pos.Wallet.Web.Pending.Util
    ( incPtxSubmitTimingPure
    , mkPtxSubmitTiming
    , ptxMarkAcknowledgedPure
    , cancelApplyingPtx
    , resetFailedPtx
    , sortPtxsChrono
    , allPendingAddresses
    , nonConfirmedTransactions
    ) where

import           Universum

import           Control.Lens ((*=), (+=), (+~), (<<*=), (<<.=))
import qualified Data.Set as Set
import Data.Reflection (give)

import           Pos.Client.Txp.Util (PendingAddresses (..))
import           Pos.Core.Common (Address)
import           Pos.Core (ProtocolConstants(..))
import           Pos.Core.Slotting (FlatSlotId, SlotId, flatSlotId)
import           Pos.Crypto (WithHash (..))
import           Pos.Txp (Tx (..), TxAux (..), TxOut (..), topsortTxs)
import           Pos.Core.Chrono (OldestFirst (..))
import           Pos.Wallet.Web.Pending.Types (PendingTx (..), PtxCondition (..), PtxCondition (..),
                                               PtxSubmitTiming (..), pstNextDelay, pstNextSlot,
                                               ptxPeerAck, ptxSubmitTiming)

mkPtxSubmitTiming :: ProtocolConstants -> SlotId -> PtxSubmitTiming
mkPtxSubmitTiming pc creationSlot = give pc $
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
    :: ProtocolConstants
    -> PtxSubmitTiming
    -> PtxSubmitTiming
incPtxSubmitTimingPure pc = give pc $ execState $ do
    curDelay <- pstNextDelay <<*= 2
    pstNextSlot . flatSlotId += curDelay

ptxMarkAcknowledgedPure :: PendingTx -> PendingTx
ptxMarkAcknowledgedPure = execState $ do
    wasAcked <- ptxPeerAck <<.= True
    unless wasAcked $ ptxSubmitTiming . pstNextDelay *= 8

-- | If given pending transaction is not yet confirmed, cancels it.
cancelApplyingPtx :: () => PendingTx -> PendingTx
cancelApplyingPtx ptx@PendingTx{..}
    | PtxApplying poolInfo <- _ptxCond =
          ptx { _ptxCond = PtxWontApply reason poolInfo
              , _ptxPeerAck = False
              }
    | otherwise = ptx
  where
    reason = "Canceled manually"

-- | If given transaction is in 'PtxWontApply' condition, sets its condition
-- to 'PtxApplying'. This allows "stuck" transactions to be resubmitted
-- again.
--
-- Has no effect for transactions in other conditions.
resetFailedPtx :: ProtocolConstants -> SlotId -> PendingTx -> PendingTx
resetFailedPtx pc curSlot ptx@PendingTx{..}
    | PtxWontApply _ poolInfo <- _ptxCond =
          ptx { _ptxCond = PtxApplying poolInfo
              , _ptxSubmitTiming = mkPtxSubmitTiming pc curSlot
              }
    | otherwise = ptx

-- | Returns the full list of "pending addresses", which are @output@ addresses
-- associated to transactions not yet persisted in the blockchain.
allPendingAddresses :: [PendingTx] -> PendingAddresses
allPendingAddresses =
    PendingAddresses . Set.unions . map grabTxOutputs . nonConfirmedTransactions
  where
    grabTxOutputs :: PendingTx -> Set.Set Address
    grabTxOutputs PendingTx{..} =
        let (TxAux tx _) = _ptxTxAux
            (UnsafeTx _ outputs _) = tx
            in Set.fromList $ map (\(TxOut a _) -> a) (toList outputs)

-- | Filters the input '[PendingTx]' to choose only the ones which are not
-- yet persisted in the blockchain.
nonConfirmedTransactions :: [PendingTx] -> [PendingTx]
nonConfirmedTransactions = filter isPending
  where
    -- | Is this 'PendingTx' really pending?
    isPending :: PendingTx -> Bool
    isPending PendingTx{..} = case _ptxCond of
        PtxInNewestBlocks _ -> False
        PtxPersisted        -> False
        _                   -> True
