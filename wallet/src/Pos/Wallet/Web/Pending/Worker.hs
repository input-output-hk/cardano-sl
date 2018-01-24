{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Pending transactions resubmition logic.

module Pos.Wallet.Web.Pending.Worker
    ( startPendingTxsResubmitter
    ) where

import           Universum

import           Control.Lens                      (has)
import           Control.Monad.Catch               (handleAll)
import           Data.Time.Units                   (Microsecond, Second, convertUnit)
import           Formatting                        (build, sformat, (%))
import           Mockable                          (delay, fork)
import           Serokell.Util.Text                (listJson)
import           System.Wlog                       (logDebug, logInfo, modifyLoggerName)

import           Pos.Client.Txp.Addresses          (MonadAddresses)
import           Pos.Communication.Protocol        (SendActions (..))
import           Pos.Configuration                 (HasNodeConfiguration,
                                                    pendingTxResubmitionPeriod,
                                                    walletTxCreationDisabled)
import           Pos.Core                          (ChainDifficulty (..), SlotId (..),
                                                    difficultyL)
import           Pos.Core.Configuration            (HasConfiguration)
import           Pos.DB.DB                         (getTipHeader)
import           Pos.Slotting                      (getNextEpochSlotDuration, onNewSlot)
import           Pos.Util.LogSafe                  (logInfoS)
import           Pos.Util.Chrono                   (getOldestFirst)
import           Pos.Wallet.SscType                (WalletSscType)
import           Pos.Wallet.Web.Mode               (MonadWalletWebMode)
import           Pos.Wallet.Web.Pending.Submission (ptxResubmissionHandler,
                                                    submitAndSavePtx)
import           Pos.Wallet.Web.Pending.Types      (PendingTx (..), PtxCondition (..),
                                                    ptxNextSubmitSlot, _PtxApplying)
import           Pos.Wallet.Web.Pending.Util       (sortPtxsChrono, usingPtxCoords)
import           Pos.Wallet.Web.State              (WalletSnapshot, askWalletDB, askWalletSnapshot,
                                                    PtxMetaUpdate (PtxIncSubmitTiming),
                                                    casPtxCondition, getPendingTx,
                                                    getPendingTxs, ptxUpdateMeta)
import           Pos.Wallet.Web.Util               (getWalletAssuredDepth)

type MonadPendings m =
    ( MonadWalletWebMode m
    , MonadAddresses m
    , HasConfiguration
    , HasNodeConfiguration
    )

processPtxInNewestBlocks :: MonadPendings m
                         => WalletSnapshot -> PendingTx -> m ()
processPtxInNewestBlocks ws PendingTx{..} = do
    let mdepth = getWalletAssuredDepth ws _ptxWallet
    tipDiff <- view difficultyL <$> getTipHeader @WalletSscType
    if | PtxInNewestBlocks ptxDiff <- _ptxCond,
         Just depth <- mdepth,
         longAgo depth ptxDiff tipDiff -> do
             db <- askWalletDB
             void $ casPtxCondition db _ptxWallet _ptxTxId _ptxCond PtxPersisted
             logInfoS $ sformat ("Transaction "%build%" got persistent") _ptxTxId
       | otherwise -> pass
  where
     longAgo depth (ChainDifficulty ptxDiff) (ChainDifficulty tipDiff) =
         ptxDiff + depth <= tipDiff

resubmitTx :: MonadPendings m => SendActions m -> WalletSnapshot -> PendingTx -> m ()
resubmitTx SendActions{..} ws ptx =
    handleAll (\_ -> pass) $ do
        logInfoS $ sformat ("Resubmitting tx "%build) (_ptxTxId ptx)
        let submissionH = ptxResubmissionHandler ptx
        submitAndSavePtx submissionH enqueueMsg ptx
        updateTiming
  where
    reportNextCheckTime =
        logInfoS .
        sformat ("Next resubmission of transaction "%build%" is scheduled at "
                %build) (_ptxTxId ptx)

    updateTiming = do
        db <- askWalletDB
        usingPtxCoords (ptxUpdateMeta db) ptx PtxIncSubmitTiming
        let nextCheck = view ptxNextSubmitSlot <$> usingPtxCoords (getPendingTx ws) ptx
        whenJust nextCheck reportNextCheckTime

-- | Distributes pending txs submition over current slot ~evenly
resubmitPtxsDuringSlot
    :: MonadPendings m
    => SendActions m -> WalletSnapshot -> [PendingTx] -> m ()
resubmitPtxsDuringSlot sendActions ws ptxs = do
    interval <- evalSubmitDelay (length ptxs)
    forM_ ptxs $ \ptx -> do
        delay interval
        fork $ resubmitTx sendActions ws ptx
  where
    submitionEta = 5 :: Second
    evalSubmitDelay toResubmitNum = do
        slotDuration <- getNextEpochSlotDuration
        let checkPeriod = max @Microsecond 0
                        $ convertUnit slotDuration - convertUnit submitionEta
        return (checkPeriod `div` fromIntegral toResubmitNum)

processPtxsToResubmit
    :: MonadPendings m
    => SendActions m -> WalletSnapshot -> SlotId -> [PendingTx] -> m ()
processPtxsToResubmit sendActions ws curSlot ptxs = do
    ptxsPerSlotLimit <- evalPtxsPerSlotLimit
    let toResubmit =
            take ptxsPerSlotLimit $
            filter ((curSlot >=) . view ptxNextSubmitSlot) $
            filter (has _PtxApplying . _ptxCond) $
            ptxs
    unless (null toResubmit) $
        logInfo $ "We are going to resubmit some transactions"
    logInfoS $ sformat fmt (map _ptxTxId toResubmit)
    resubmitPtxsDuringSlot sendActions ws toResubmit
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
    => SendActions m -> WalletSnapshot -> SlotId -> [PendingTx] -> m ()
processPtxs sendActions ws curSlot ptxs = do
    mapM_ (processPtxInNewestBlocks ws) ptxs

    if walletTxCreationDisabled
    then logDebug "Transaction resubmission is disabled"
    else processPtxsToResubmit sendActions ws curSlot ptxs

processPtxsOnSlot
    :: MonadPendings m
    => SendActions m -> SlotId -> m ()
processPtxsOnSlot sendActions curSlot = do
    ws <- askWalletSnapshot
    let ptxs = getPendingTxs ws
    let sortedPtxs = getOldestFirst $ sortPtxsChrono ptxs
    processPtxs sendActions ws curSlot sortedPtxs

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
