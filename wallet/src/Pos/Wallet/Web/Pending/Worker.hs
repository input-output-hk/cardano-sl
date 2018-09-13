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
import           Serokell.Util (enumerate, listJson)

import           Pos.Chain.Txp (TxAux, TxpConfiguration)
import           Pos.Client.Txp.Addresses (MonadAddresses)
import           Pos.Client.Txp.Network (TxMode)
import           Pos.Configuration (HasNodeConfiguration,
                     pendingTxResubmitionPeriod, walletTxCreationDisabled)
import           Pos.Core as Core (ChainDifficulty (..), Config (..),
                     SlotId (..), configEpochSlots, difficultyL)
import           Pos.Core.Chrono (getOldestFirst)
import           Pos.Core.Conc (delay, forConcurrently)
import qualified Pos.DB.BlockIndex as DB
import           Pos.DB.Class (MonadDBRead)
import           Pos.Infra.Recovery.Info (MonadRecoveryInfo)
import           Pos.Infra.Reporting (MonadReporting)
import           Pos.Infra.Shutdown (HasShutdownContext)
import           Pos.Infra.Slotting (MonadSlots, OnNewSlotParams (..),
                     defaultOnNewSlotParams, getNextEpochSlotDuration,
                     onNewSlot)
import           Pos.Infra.Util.LogSafe (logInfoSP, secretOnlyF, secureListF)
import           Pos.Util.Wlog (logDebug, logInfo, modifyLoggerName)
import           Pos.Wallet.Web.Pending.Functions (usingPtxCoords)
import           Pos.Wallet.Web.Pending.Submission (ptxResubmissionHandler,
                     submitAndSavePtx)
import           Pos.Wallet.Web.Pending.Types (PendingTx (..),
                     PtxCondition (..), ptxNextSubmitSlot, _PtxApplying)
import           Pos.Wallet.Web.Pending.Util (sortPtxsChrono)
import           Pos.Wallet.Web.State (PtxMetaUpdate (PtxIncSubmitTiming),
                     WalletDB, casPtxCondition, getPendingTx, getPendingTxs,
                     getWalletSnapshot, ptxUpdateMeta)
import           Pos.Wallet.Web.Util (getWalletAssuredDepth)

type MonadPendings ctx m =
    ( TxMode m
    , MonadAddresses m
    , MonadDBRead m
    , MonadRecoveryInfo ctx m
    , MonadReporting m
    , HasShutdownContext ctx
    , MonadSlots ctx m
    , HasNodeConfiguration
    )

processPtxInNewestBlocks :: MonadPendings ctx m => WalletDB -> PendingTx -> m ()
processPtxInNewestBlocks db PendingTx{..} = do
    ws <- getWalletSnapshot db
    tipDiff <- view difficultyL <$> DB.getTipHeader
    if | PtxInNewestBlocks ptxDiff <- _ptxCond,
         Just depth <- getWalletAssuredDepth ws _ptxWallet,
         longAgo depth ptxDiff tipDiff -> do
             void $ casPtxCondition db _ptxWallet _ptxTxId _ptxCond PtxPersisted
             logInfoSP $ \sl -> sformat ("Transaction "%secretOnlyF sl build%" got persistent") _ptxTxId
       | otherwise -> pass
  where
     longAgo depth (ChainDifficulty ptxDiff) (ChainDifficulty tipDiff) =
         ptxDiff + depth <= tipDiff

resubmitTx :: MonadPendings ctx m
           => Core.Config
           -> TxpConfiguration
           -> WalletDB
           -> (TxAux -> m Bool)
           -> PendingTx
           -> m ()
resubmitTx coreConfig txpConfig db submitTx ptx =
    handleAny (\_ -> pass) $ do
        logInfoSP $ \sl -> sformat ("Resubmitting tx "%secretOnlyF sl build) (_ptxTxId ptx)
        let submissionH = ptxResubmissionHandler db ptx
        submitAndSavePtx coreConfig txpConfig db submitTx submissionH ptx
        updateTiming
  where
    reportNextCheckTime time = logInfoSP $ \sl -> sformat
        ( "Next resubmission of transaction "
        % secretOnlyF sl build
        % " is scheduled at "
        % build
        )
        (_ptxTxId ptx)
        time

    updateTiming = do
        usingPtxCoords
            (ptxUpdateMeta (configProtocolConstants coreConfig) db)
            ptx
            PtxIncSubmitTiming
        ws <- getWalletSnapshot db
        let nextCheck =
                view ptxNextSubmitSlot <$> usingPtxCoords (getPendingTx ws) ptx
        whenJust nextCheck reportNextCheckTime

-- | Distributes pending txs submition over current slot ~evenly
resubmitPtxsDuringSlot
    :: MonadPendings ctx m
    => Core.Config
    -> TxpConfiguration
    -> WalletDB
    -> (TxAux -> m Bool)
    -> [PendingTx]
    -> m ()
resubmitPtxsDuringSlot coreConfig txpConfig db submitTx ptxs = do
    interval <- evalSubmitDelay (length ptxs)
    void . forConcurrently (enumerate ptxs) $ \(i, ptx) -> do
        delay (interval * i)
        resubmitTx coreConfig txpConfig db submitTx ptx
  where
    submitionEta = 5 :: Second
    evalSubmitDelay toResubmitNum = do
        slotDuration <- getNextEpochSlotDuration
        let checkPeriod = max @Microsecond 0
                        $ convertUnit slotDuration - convertUnit submitionEta
        return (checkPeriod `div` fromIntegral toResubmitNum)

processPtxsToResubmit
    :: MonadPendings ctx m
    => Core.Config
    -> TxpConfiguration
    -> WalletDB
    -> (TxAux -> m Bool)
    -> SlotId
    -> [PendingTx]
    -> m ()
processPtxsToResubmit coreConfig txpConfig db submitTx _curSlot ptxs = do
    ptxsPerSlotLimit <- evalPtxsPerSlotLimit
    let toResubmit =
            take (min 1 ptxsPerSlotLimit) $  -- for now the limit will be 1,
                                             -- though properly “min 1”
                                             -- shouldn't be needed
            filter (has _PtxApplying . _ptxCond) $
            ptxs
    unless (null toResubmit) $ do
        logInfo $ "We are going to resubmit some transactions"
        logInfoSP $ \sl -> sformat (fmt sl) (map _ptxTxId toResubmit)
    when (null toResubmit) $
        logDebug "There are no transactions to resubmit"
    resubmitPtxsDuringSlot coreConfig txpConfig db submitTx toResubmit
  where
    fmt sl = "Transactions to resubmit on current slot: "%secureListF sl listJson
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
    => Core.Config
    -> TxpConfiguration
    -> WalletDB
    -> (TxAux -> m Bool)
    -> SlotId
    -> [PendingTx]
    -> m ()
processPtxs coreConfig txpConfig db submitTx curSlot ptxs = do
    mapM_ (processPtxInNewestBlocks db) ptxs
    if walletTxCreationDisabled
    then logDebug "Transaction resubmission is disabled"
    else processPtxsToResubmit coreConfig txpConfig db submitTx curSlot ptxs

processPtxsOnSlot
    :: MonadPendings ctx m
    => Core.Config
    -> TxpConfiguration
    -> WalletDB
    -> (TxAux -> m Bool)
    -> SlotId
    -> m ()
processPtxsOnSlot coreConfig txpConfig db submitTx curSlot = do
    ws <- getWalletSnapshot db
    let ptxs = getPendingTxs ws
    let sortedPtxs = getOldestFirst $ sortPtxsChrono ptxs
    processPtxs coreConfig txpConfig db submitTx curSlot sortedPtxs

-- | On each slot this takes several pending transactions and resubmits them if
-- needed and possible.
startPendingTxsResubmitter
    :: MonadPendings ctx m
    => Core.Config
    -> TxpConfiguration
    -> WalletDB
    -> (TxAux -> m Bool)
    -> m ()
startPendingTxsResubmitter coreConfig txpConfig db submitTx =
    setLogger $ onNewSlot
        (configEpochSlots coreConfig)
        onsp
        (processPtxsOnSlot coreConfig txpConfig db submitTx)
  where
    setLogger = modifyLoggerName (<> "tx" <> "resubmitter")
    onsp :: OnNewSlotParams
    onsp = defaultOnNewSlotParams { onspStartImmediately = False }
