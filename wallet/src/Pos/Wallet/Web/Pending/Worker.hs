{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Pending transactions resubmition logic.

module Pos.Wallet.Web.Pending.Worker
    ( startPendingTxsResubmitter
    ) where

import           Universum

import           Control.Lens                      (has)
import           Data.Time.Units                   (Microsecond, Second, convertUnit)
import           Formatting                        (build, sformat, shown, (%))
import           Mockable                          (delay, fork)
import           Serokell.Util.Text                (listJson)
import           System.Wlog                       (logInfo, logWarning, modifyLoggerName)

import           Pos.Client.Txp.Addresses          (MonadAddresses)
import           Pos.Communication.Protocol        (SendActions (..))
import           Pos.Constants                     (pendingTxResubmitionPeriod)
import           Pos.Core                          (ChainDifficulty (..), SlotId (..),
                                                    difficultyL)
import           Pos.Core.Context                  (HasCoreConstants)
import           Pos.Crypto                        (WithHash (..))
import           Pos.DB.DB                         (getTipHeader)
import           Pos.Slotting                      (getNextEpochSlotDuration, onNewSlot)
import           Pos.Txp                           (TxAux (..), topsortTxs)
import           Pos.Wallet.SscType                (WalletSscType)
import           Pos.Wallet.Web.Mode               (MonadWalletWebMode)
import           Pos.Wallet.Web.Pending.Submission (ptxResubmissionHandler,
                                                    submitAndSavePtx)
import           Pos.Wallet.Web.Pending.Types      (PendingTx (..), PtxCondition (..),
                                                    PtxSubmitTiming (..), _PtxApplying)
import           Pos.Wallet.Web.State              (PtxMetaUpdate (PtxIncSubmitTiming),
                                                    casPtxCondition, getPendingTxs,
                                                    ptxUpdateMeta)
import           Pos.Wallet.Web.Util               (getWalletAssuredDepth)

type MonadPendings m =
    ( MonadWalletWebMode m
    , MonadAddresses m
    , HasCoreConstants
    )

processPtxInNewestBlocks :: MonadPendings m => PendingTx -> m ()
processPtxInNewestBlocks PendingTx{..} = do
    mdepth <- getWalletAssuredDepth _ptxWallet
    tipDiff <- view difficultyL <$> getTipHeader @WalletSscType
    if | PtxInNewestBlocks ptxDiff <- _ptxCond,
         Just depth <- mdepth,
         longAgo depth ptxDiff tipDiff -> do
             void $ casPtxCondition _ptxWallet _ptxTxId _ptxCond PtxPersisted
             logInfo $ sformat ("Transaction "%build%" got persistent") _ptxTxId
       | otherwise -> pass
  where
     longAgo depth (ChainDifficulty ptxDiff) (ChainDifficulty tipDiff) =
         ptxDiff + depth <= tipDiff

resubmitTx :: MonadPendings m => SendActions m -> PendingTx -> m ()
resubmitTx SendActions{..} ptx@PendingTx{..} = do
    logInfo $ sformat ("Resubmitting tx "%build) _ptxTxId
    let submissionH = ptxResubmissionHandler ptx
    submitAndSavePtx submissionH enqueueMsg ptx
        `catchAll` reportUnhandledError
    ptxUpdateMeta _ptxWallet _ptxTxId PtxIncSubmitTiming
  where
    reportUnhandledError =
        logWarning .
        sformat ("Unexpected unhandled error from resubmission: "%shown)

-- | Distributes pending txs submition over current slot ~evenly
resubmitPtxsDuringSlot
    :: MonadPendings m
    => SendActions m -> [PendingTx] -> m ()
resubmitPtxsDuringSlot sendActions ptxs = do
    interval <- evalSubmitDelay (length ptxs)
    forM_ ptxs $ \ptx -> do
        delay interval
        fork $ resubmitTx sendActions ptx
  where
    submitionEta = 5 :: Second
    evalSubmitDelay toResubmitNum = do
        slotDuration <- getNextEpochSlotDuration
        let checkPeriod = max @Microsecond 0
                        $ convertUnit slotDuration - convertUnit submitionEta
        return (checkPeriod `div` fromIntegral toResubmitNum)

processPtxsToResubmit
    :: MonadPendings m
    => SendActions m -> SlotId -> [PendingTx] -> m ()
processPtxsToResubmit sendActions curSlot ptxs = do
    ptxsPerSlotLimit <- evalPtxsPerSlotLimit
    let toResubmit =
            take ptxsPerSlotLimit $
            filter ((curSlot >=) . _pstNextSlot . _ptxSubmitTiming) $
            filter (has _PtxApplying . _ptxCond)
            ptxs
    logInfo $ sformat fmt (map _ptxTxId toResubmit)
    resubmitPtxsDuringSlot sendActions toResubmit
  where
    fmt = "Transactions to resubmit on current slot: "%listJson
    evalPtxsPerSlotLimit = do
        slotDuration <- getNextEpochSlotDuration
        let limit = fromIntegral $
                convertUnit slotDuration `div` pendingTxResubmitionPeriod
        when (limit <= 0) $
            logInfo "'pendingTxResubmitionPeriod' is larger than slot duration,\
                    \ won't resubmit any pending transaction"
        return limit

-- | Checks and updates state of given pending transactions, resubmitting them
-- if needed.
processPtxs
    :: MonadPendings m
    => SendActions m -> SlotId -> [PendingTx] -> m ()
processPtxs sendActions curSlot ptxs = do
    mapM_ processPtxInNewestBlocks ptxs
    processPtxsToResubmit sendActions curSlot ptxs

processPtxsOnSlot
    :: MonadPendings m
    => SendActions m -> SlotId -> m ()
processPtxsOnSlot sendActions curSlot = do
    ptxs <- getPendingTxs
    let sortedPtxs =
            sortWith _ptxCreationSlot $
            flip fromMaybe =<< topsortTxs wHash $
            ptxs

    processPtxs sendActions curSlot sortedPtxs
  where
    wHash PendingTx{..} = WithHash (taTx _ptxTxAux) _ptxTxId

-- | On each slot this takes several pending transactions and resubmits them if
-- needed and possible.
startPendingTxsResubmitter
    :: MonadPendings m
    => SendActions m -> m ()
startPendingTxsResubmitter sa =
    void . fork . setLogger $
    onNewSlot False (processPtxsOnSlot sa)
  where
    setLogger = modifyLoggerName (<> "tx" <> "resubmitter")
