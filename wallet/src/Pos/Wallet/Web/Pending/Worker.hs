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
import           System.Wlog                       (logInfo, modifyLoggerName)

import           Pos.Client.Txp.Addresses          (MonadAddresses)
import           Pos.Communication.Protocol        (SendActions (..))
import           Pos.Configuration                 (HasNodeConfiguration,
                                                    pendingTxResubmitionPeriod)
import           Pos.Core                          (ChainDifficulty (..), SlotId (..),
                                                    difficultyL)
import           Pos.Core.Configuration            (HasConfiguration)
import           Pos.Crypto                        (WithHash (..))
import           Pos.DB.DB                         (getTipHeader)
import           Pos.Slotting                      (getNextEpochSlotDuration, onNewSlot)
import           Pos.Txp                           (TxAux (..), topsortTxs)
import           Pos.Util.LogSafe                  (logInfoSP, secretOnlyF, secureListF)
import           Pos.Wallet.SscType                (WalletSscType)
import           Pos.Wallet.Web.Mode               (MonadWalletWebMode)
import           Pos.Wallet.Web.Pending.Submission (ptxResubmissionHandler,
                                                    submitAndSavePtx)
import           Pos.Wallet.Web.Pending.Types      (PendingTx (..), PtxCondition (..),
                                                    ptxNextSubmitSlot, _PtxApplying)
import           Pos.Wallet.Web.Pending.Util       (usingPtxCoords)
import           Pos.Wallet.Web.State              (PtxMetaUpdate (PtxIncSubmitTiming),
                                                    casPtxCondition, getPendingTx,
                                                    getPendingTxs, ptxUpdateMeta)
import           Pos.Wallet.Web.Util               (getWalletAssuredDepth)

type MonadPendings m =
    ( MonadWalletWebMode m
    , MonadAddresses m
    , HasConfiguration
    , HasNodeConfiguration
    )

processPtxInNewestBlocks :: MonadPendings m => PendingTx -> m ()
processPtxInNewestBlocks PendingTx{..} = do
    mdepth <- getWalletAssuredDepth _ptxWallet
    tipDiff <- view difficultyL <$> getTipHeader @WalletSscType
    if | PtxInNewestBlocks ptxDiff <- _ptxCond,
         Just depth <- mdepth,
         longAgo depth ptxDiff tipDiff -> do
             void $ casPtxCondition _ptxWallet _ptxTxId _ptxCond PtxPersisted
             logInfoSP $ \sl -> sformat ("Transaction "%secretOnlyF sl build%" got persistent") _ptxTxId
       | otherwise -> pass
  where
     longAgo depth (ChainDifficulty ptxDiff) (ChainDifficulty tipDiff) =
         ptxDiff + depth <= tipDiff

resubmitTx :: MonadPendings m => SendActions m -> PendingTx -> m ()
resubmitTx SendActions{..} ptx =
    handleAll (\_ -> pass) $ do
        logInfoSP $ \sl -> sformat ("Resubmitting tx "%secretOnlyF sl build) (_ptxTxId ptx)
        let submissionH = ptxResubmissionHandler ptx
        submitAndSavePtx submissionH enqueueMsg ptx
        updateTiming
  where
    reportNextCheckTime time =
        logInfoSP $ \sl ->
        sformat ("Next resubmission of transaction "%secretOnlyF sl build%" is scheduled at "
                %build) (_ptxTxId ptx) time

    updateTiming = do
        usingPtxCoords ptxUpdateMeta ptx PtxIncSubmitTiming
        nextCheck <- view ptxNextSubmitSlot <<$>> usingPtxCoords getPendingTx ptx
        whenJust nextCheck reportNextCheckTime

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
processPtxsToResubmit sendActions _curSlot ptxs = do
    ptxsPerSlotLimit <- evalPtxsPerSlotLimit
    let toResubmit =
            take (min 1 ptxsPerSlotLimit) $  -- for now the limit will be 1,
                                             -- though properly “min 1”
                                             -- shouldn't be needed
            filter (has _PtxApplying . _ptxCond) $
            ptxs
    unless (null toResubmit) $
        logInfo $ "We are going to resubmit some transactions"
    logInfoSP $ \sl -> sformat (fmt sl) (map _ptxTxId toResubmit)
    resubmitPtxsDuringSlot sendActions toResubmit
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
