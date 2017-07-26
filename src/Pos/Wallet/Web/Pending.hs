-- | Pending transactions resubmition logic.

module Pos.Wallet.Web.Pending
    ( startPendingTxsResubmitter
    ) where

import Universum

import Mockable (delay, fork)

import           Pos.Core             (SlotId, getSlotCount)
import           Pos.Communication.Tx (submitAndSaveTx)
import           Pos.Constants       (slotSecurityParam)
import           Pos.Slotting        (getLastKnownSlotDuration, onNewSlot)
import           Pos.Core.Slotting    (flattenSlotId)
import           Pos.Txp.Pending         (MonadPendingTxs (..), PendingTx (..),
                                          TxPendingState (..), ptxCreationSlot,
                                          ptxExpireSlot)
import           Pos.Client.Txp.Balances (MonadBalances)
import           Pos.WorkMode.Class  (WorkMode)
import Pos.Discovery.Class ( getPeers)
import Pos.Communication (SendActions)
import           Pos.Client.Txp.History  (MonadTxHistory)
import Data.Time.Units (Second, convertUnit)

type MonadPendings ssc m =
    ( MonadBalances m
    , MonadTxHistory ssc m
    , MonadPendingTxs m
    )

canSubmitPtx
    :: (WorkMode ssc ctx m, MonadPendings ssc m)
    => PendingTx -> m Bool
canSubmitPtx ptx@PendingTx{..} = do
    present   <- checkTxIsInMempool ptTxAux
    applicable <- checkTxIsApplicable ptTxAux
    if | present    -> return False
       | applicable -> return True
       | otherwise  -> False <$ setPendingTx ptx TxWon'tSend
  where
    checkTxIsInMempool = undefined
    checkTxIsApplicable = undefined

whetherToResubmitPtx
    :: (WorkMode ssc ctx m, MonadPendings ssc m)
    => SlotId -> PendingTx -> m Bool
whetherToResubmitPtx curSlot ptx = do
    inBlocks <- isPtxInRecentBlocks ptx
    if | and
       [ inBlocks
       , flattenSlotId (ptxCreationSlot ptx) + getSlotCount slotSecurityParam
           < flattenSlotId curSlot
       ] ->
           False <$ removePendingTx ptx
       | expired ->
           False <$ setPendingTx ptx TxWon'tSend
       | not inBlocks ->
           canSubmitPtx ptx
       | otherwise    ->
           return False
  where
    isPtxInRecentBlocks = undefined
    expired = ptxExpireSlot ptx <= curSlot

whetherCheckPtxOnSlot :: SlotId -> PendingTx -> Bool
whetherCheckPtxOnSlot curSlot ptx =
    -- TODO [CSM-256]: move 3 in constants?
    flattenSlotId (ptxCreationSlot ptx) + 3 < flattenSlotId curSlot

-- | On each slot takes several pending transactions (using
-- 'whetherToResubmitPtx' rule) and resubmits them if needed & possible.
-- It's not /worker/, because it requires some constraints, natural only for
-- wallet environment.
startPendingTxsResubmitter
    :: (WorkMode ssc ctx m, MonadPendings ssc m)
    => SendActions m -> m ()
startPendingTxsResubmitter sendActions =
    onNewSlot False $ \curSlot -> do
        pendingTxs <- getPendingTxs TxApplying
        let ptxsToCheck =
                filter (whetherCheckPtxOnSlot curSlot) pendingTxs
        -- FIXME [CSM-256]: add limit on number of resent transactions per slot or on speed?
        toResubmit <- filterM (whetherToResubmitPtx curSlot) ptxsToCheck

        -- distribute txs submition over current slot ~evenly
        interval <- evalSubmitDelay (length toResubmit)
        na <- toList <$> getPeers
        forM_ toResubmit $ \PendingTx{..} -> do
            delay interval
            fork $ submitAndSaveTx sendActions na ptTxAux
  where
    submitionEta = 5 :: Second
    evalSubmitDelay toResubmitNum = do
        slotDuration <- getLastKnownSlotDuration
        let checkPeriod = max 0 $ slotDuration - convertUnit submitionEta
        return (checkPeriod `div` fromIntegral toResubmitNum)

