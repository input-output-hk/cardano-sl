{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Pending transactions resubmission logic.

module Pos.Wallet.Web.Pending.Worker
    ( startPendingTxsResubmitter
    ) where

import           Universum

import           Control.Exception.Safe (handleAny)
import           Control.Lens (has)
import           Data.Time.Units (Microsecond, Second, convertUnit)
import           Formatting (build, sformat, (%))
import           Mockable (delay, forConcurrently)
import           Serokell.Util (enumerate, listJson)
import           System.Wlog (logDebug, logInfo, modifyLoggerName)

import           Pos.Client.Txp.Addresses (MonadAddresses)
import           Pos.Client.Txp.Network (TxMode)
import           Pos.Configuration (HasNodeConfiguration, pendingTxResubmitionPeriod,
                                    walletTxCreationDisabled)
import           Pos.Core (ChainDifficulty (..), SlotId (..), difficultyL)
import           Pos.Core.Configuration (HasConfiguration)
import qualified Pos.DB.BlockIndex as DB
import           Pos.DB.Class (MonadDBRead)
import           Pos.Recovery.Info (MonadRecoveryInfo)
import           Pos.Reporting (MonadReporting)
import           Pos.Shutdown (HasShutdownContext)
import           Pos.Slotting (MonadSlots, getNextEpochSlotDuration, onNewSlot)
import           Pos.Util.Chrono (getOldestFirst)
import           Pos.Util.LogSafe (logDebugS, logInfoS)
import           Pos.Wallet.Web.Networking (MonadWalletSendActions)
import           Pos.Wallet.Web.Pending.Functions (usingPtxCoords)
import           Pos.Wallet.Web.Pending.Submission (ptxResubmissionHandler, submitAndSavePtx)
import           Pos.Wallet.Web.Pending.Types (PendingTx (..), PtxCondition (..), ptxNextSubmitSlot,
                                               _PtxApplying)
import           Pos.Wallet.Web.Pending.Util (sortPtxsChrono)
import           Pos.Wallet.Web.State (MonadWalletDB, PtxMetaUpdate (PtxIncSubmitTiming),
                                       casPtxCondition, getPendingTx, getPendingTxs, ptxUpdateMeta)
import           Pos.Wallet.Web.Util (getWalletAssuredDepth)

type MonadPendings ctx m =
    ( TxMode m
    , MonadAddresses m
    , MonadDBRead m
    , MonadRecoveryInfo m
    , MonadReporting ctx m
    , HasShutdownContext ctx
    , MonadSlots ctx m
    , MonadWalletSendActions m
    , MonadWalletDB ctx m
    , HasConfiguration
    , HasNodeConfiguration
    )

processPtxInNewestBlocks :: MonadPendings ctx m => PendingTx -> m ()
processPtxInNewestBlocks PendingTx{..} = do
    mdepth <- getWalletAssuredDepth _ptxWallet
    tipDiff <- view difficultyL <$> DB.getTipHeader
    if | PtxInNewestBlocks ptxDiff <- _ptxCond,
         Just depth <- mdepth,
         longAgo depth ptxDiff tipDiff -> do
             void $ casPtxCondition _ptxWallet _ptxTxId _ptxCond PtxPersisted
             logInfoS $ sformat ("Transaction "%build%" got persistent") _ptxTxId
       | otherwise -> pass
  where
     longAgo depth (ChainDifficulty ptxDiff) (ChainDifficulty tipDiff) =
         ptxDiff + depth <= tipDiff

resubmitTx :: MonadPendings ctx m => PendingTx -> m ()
resubmitTx ptx =
    handleAny (\_ -> pass) $ do
        logInfoS $ sformat ("Resubmitting tx "%build) (_ptxTxId ptx)
        let submissionH = ptxResubmissionHandler ptx
        submitAndSavePtx submissionH ptx
        updateTiming
  where
    reportNextCheckTime =
        logInfoS .
        sformat ("Next resubmission of transaction "%build%" is scheduled at "
                %build) (_ptxTxId ptx)

    updateTiming = do
        usingPtxCoords ptxUpdateMeta ptx PtxIncSubmitTiming
        nextCheck <- view ptxNextSubmitSlot <<$>> usingPtxCoords getPendingTx ptx
        whenJust nextCheck reportNextCheckTime

-- | Distributes pending txs submition over current slot ~evenly
resubmitPtxsDuringSlot
    :: MonadPendings ctx m
    => [PendingTx] -> m ()
resubmitPtxsDuringSlot ptxs = do
    interval <- evalSubmitDelay (length ptxs)
    void . forConcurrently (enumerate ptxs) $ \(i, ptx) -> do
        delay (interval * i)
        resubmitTx ptx
  where
    submitionEta = 5 :: Second
    evalSubmitDelay toResubmitNum = do
        slotDuration <- getNextEpochSlotDuration
        let checkPeriod = max @Microsecond 0
                        $ convertUnit slotDuration - convertUnit submitionEta
        return (checkPeriod `div` fromIntegral toResubmitNum)

processPtxsToResubmit
    :: MonadPendings ctx m
    => SlotId -> [PendingTx] -> m ()
processPtxsToResubmit _curSlot ptxs = do
    ptxsPerSlotLimit <- evalPtxsPerSlotLimit
    let toResubmit =
            take (min 1 ptxsPerSlotLimit) $  -- for now the limit will be 1,
                                             -- though properly “min 1”
                                             -- shouldn't be needed
            filter (has _PtxApplying . _ptxCond) $
            ptxs
    unless (null toResubmit) $ do
        logInfo $ "We are going to resubmit some transactions"
        logInfoS $ sformat fmt (map _ptxTxId toResubmit)
    when (null toResubmit) $
        logDebugS "There are no transactions to resubmit"
    resubmitPtxsDuringSlot toResubmit
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
    :: MonadPendings ctx m
    => SlotId -> [PendingTx] -> m ()
processPtxs curSlot ptxs = do
    mapM_ processPtxInNewestBlocks ptxs
    if walletTxCreationDisabled
    then logDebug "Transaction resubmission is disabled"
    else processPtxsToResubmit curSlot ptxs

processPtxsOnSlot
    :: MonadPendings ctx m
    => SlotId -> m ()
processPtxsOnSlot curSlot = do
    ptxs <- getPendingTxs
    let sortedPtxs = getOldestFirst $ sortPtxsChrono ptxs
    processPtxs curSlot sortedPtxs

-- | On each slot this takes several pending transactions and resubmits them if
-- needed and possible.
startPendingTxsResubmitter
    :: MonadPendings ctx m
    => m ()
startPendingTxsResubmitter = setLogger $ onNewSlot False processPtxsOnSlot
  where
    setLogger = modifyLoggerName (<> "tx" <> "resubmitter")
