{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Pending transactions resubmition logic.

module Pos.Wallet.Web.Pending.Worker
    ( startPendingTxsResubmitter
    ) where

import           Universum

import           Control.Lens                 (has)
import           Control.Monad.Catch          (Handler (..), catches)
import           Data.Time.Units              (Microsecond, Second, convertUnit)
import           Formatting                   (build, sformat, shown, stext, (%))
import           Mockable                     (delay, fork)
import           Serokell.Util.Text           (listJson)
import           System.Wlog                  (logInfo, logWarning, modifyLoggerName)

import           Pos.Client.Txp.Addresses     (MonadAddresses)
import           Pos.Communication            (submitAndSaveTx)
import           Pos.Communication.Protocol   (SendActions (..))
import           Pos.Constants                (pendingTxResubmitionPeriod)
import           Pos.Core                     (ChainDifficulty (..), FlatSlotId,
                                               SlotId (..), difficultyL)
import           Pos.Core.Context             (HasCoreConstants)
import           Pos.Core.Slotting            (flattenSlotId)
import           Pos.Crypto                   (WithHash (..))
import           Pos.DB.DB                    (getTipHeader)
import           Pos.Slotting                 (getLastKnownSlotDuration, onNewSlot)
import           Pos.Txp                      (ToilVerFailure (..), TxAux (..),
                                               topsortTxs)
import           Pos.Wallet.SscType           (WalletSscType)
import           Pos.Wallet.Web.Mode          (MonadWalletWebMode)
import           Pos.Wallet.Web.Pending.Types (PendingTx (..), PtxCondition (..),
                                               _PtxApplying)
import           Pos.Wallet.Web.Pending.Util  (isReclaimableFailure)
import           Pos.Wallet.Web.State         (casPtxCondition, getPendingTxs)
import           Pos.Wallet.Web.Util          (getWalletAssuredDepth)

type MonadPendings m =
    ( MonadWalletWebMode m
    , MonadAddresses m
    , HasCoreConstants
    )

-- | What should happen with existing pending transaction if attempt
-- to resubmit it failed.
processPtxFailure :: MonadPendings m => PendingTx -> ToilVerFailure -> m ()
processPtxFailure PendingTx{..} e
    | PtxApplying poolInfo <- _ptxCond =
        unless (isReclaimableFailure e) $ do
            let newCond = PtxWontApply (sformat build e) poolInfo
            void $ casPtxCondition _ptxWallet _ptxTxId _ptxCond newCond
            logInfo $ sformat ("Pending transaction "%build%" was canceled")
                    _ptxTxId
    | otherwise =
        logWarning $
        sformat ("Processing failure of "%build%" resubmission, but \
                 \this transaction has unexpected condition "%build)
                _ptxTxId _ptxCond


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

-- | 'True' for slots which are equal to
-- @ptxCreationSlot + initialDelay + furtherDelay * k@ for some integer @k@
whetherCheckPtxOnSlot :: HasCoreConstants => SlotId -> PendingTx -> Bool
whetherCheckPtxOnSlot (flattenSlotId -> curSlot) ptx = do
    let ptxSlot = flattenSlotId (_ptxCreationSlot ptx)
        checkStartSlot = ptxSlot + initialDelay
    and [ curSlot >= checkStartSlot
        , ((curSlot - checkStartSlot) `mod` furtherDelay) == 0
        ]
  where
    initialDelay = 3 :: FlatSlotId
    furtherDelay = 1 :: FlatSlotId

resubmitTx :: MonadPendings m => SendActions m -> PendingTx -> m ()
resubmitTx SendActions{..} ptx@PendingTx{..} = do
    logInfo $ sformat ("Resubmitting tx "%build) _ptxTxId
    -- FIXME [CSM-256] Doesn't it introduce a race condition?
    void (submitAndSaveTx enqueueMsg _ptxTxAux) `catches` handlers
  where
    handlers =
        [ Handler $ \e -> do
            reportFail "Failed to resubmit tx" e
            processPtxFailure ptx e

        , Handler $ \(SomeException e) ->
            reportFail "Failed to resubmit tx, will try again later" e
        ]
    reportFail :: MonadPendings m => Exception e => Text -> e -> m ()
    reportFail desc =
        logInfo . sformat (stext%" "%build%": "%shown) desc _ptxTxId

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
        slotDuration <- getLastKnownSlotDuration
        let checkPeriod = max 0 $ slotDuration - convertUnit submitionEta
        return (checkPeriod `div` fromIntegral toResubmitNum)

processPtxsToResubmit
    :: MonadPendings m
    => SendActions m -> [PendingTx] -> m ()
processPtxsToResubmit sendActions ptxs = do
    ptxsPerSlotLimit <- evalPtxsPerSlotLimit
    let toResubmit =
            take ptxsPerSlotLimit $
            filter (has _PtxApplying . _ptxCond)
            ptxs
    logInfo $ sformat fmt (map _ptxTxId toResubmit)
    resubmitPtxsDuringSlot sendActions toResubmit
  where
    fmt = "Transactions to resubmit on current slot: "%listJson
    evalPtxsPerSlotLimit = do
        slotDuration <- getLastKnownSlotDuration
        let limit = fromIntegral @Microsecond $
                convertUnit slotDuration `div` convertUnit pendingTxResubmitionPeriod
        when (limit <= 0) $
            logInfo "'pendingTxResubmitionPeriod' is larger than slot duration,\
                    \ won't resubmit any pending transaction"
        return limit

-- | Checks and updates state of given pending transactions, resubmitting them
-- if needed.
processPtxs
    :: MonadPendings m
    => SendActions m -> [PendingTx] -> m ()
processPtxs sendActions ptxs = do
    mapM_ processPtxInNewestBlocks ptxs
    processPtxsToResubmit sendActions ptxs

processPtxsOnSlot
    :: MonadPendings m
    => SendActions m -> SlotId -> m ()
processPtxsOnSlot sendActions curSlot = do
    ptxs <- getPendingTxs
    let selectedPtxs =
            sortWith _ptxCreationSlot $
            flip fromMaybe =<< topsortTxs wHash $
            filter (whetherCheckPtxOnSlot curSlot) $
            ptxs

    processPtxs sendActions selectedPtxs
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
