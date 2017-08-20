{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Pending transactions resubmition logic.

module Pos.Wallet.Web.Pending.Worker
    ( startPendingTxsResubmitter
    ) where

import           Universum

import           Control.Monad.Catch          (Handler (..), catches)
import           Data.Time.Units              (Second, convertUnit)
import           Formatting                   (build, sformat, shown, stext, (%))
import           Mockable                     (delay, fork)
import           Serokell.Util.Text           (listJson)
import           System.Wlog                  (logInfo, modifyLoggerName)

import           Pos.Client.Txp.Addresses     (MonadAddresses)
import           Pos.Communication            (submitAndSaveTx)
import           Pos.Communication.Protocol   (SendActions (..))
import           Pos.Constants                (pendingTxResubmitionPeriod)
import           Pos.Core                     (FlatSlotId, SlotId (..), getBlockCount)
import           Pos.Core.Context             (HasCoreConstants)
import           Pos.Core.Slotting            (flattenSlotId)
import           Pos.Crypto                   (WithHash (..))
import           Pos.Slotting                 (getLastKnownSlotDuration, onNewSlot)
import           Pos.Txp                      (ToilVerFailure (..), TxAux (..),
                                               topsortTxs)
import           Pos.Wallet.Web.Mode          (MonadWalletWebMode)
import           Pos.Wallet.Web.Pending.Types (PendingTx (..), PtxCondition (..))
import           Pos.Wallet.Web.Pending.Util  (isReclaimableFailure)
import           Pos.Wallet.Web.State         (casPtxCondition, countDownPtxAttempts,
                                               getPendingTxs, setPtxCondition)
import           Pos.Wallet.Web.Util          (getWalletAssuredDepth)

type MonadPendings m =
    ( MonadWalletWebMode m
    , MonadAddresses m
    , HasCoreConstants
    )

-- | What should happen with existing pending transaction if attempt
-- to resubmit it failed.
processPtxFailure :: MonadPendings m => PendingTx -> ToilVerFailure -> m ()
processPtxFailure PendingTx{..} e =
    if isReclaimableFailure e
        then countDownPtxAttempts _ptxWallet _ptxTxId
        else do
            let newCond = PtxWontApply (sformat build e)
            void $ casPtxCondition _ptxWallet _ptxTxId PtxApplying newCond
            logInfo $ sformat ("Transaction "%build%" was canceled") _ptxTxId

processPtxInUpperBlocks :: MonadPendings m => SlotId -> PendingTx -> m ()
processPtxInUpperBlocks curSlot PendingTx{..} = do
    mdepth <- getWalletAssuredDepth _ptxWallet
    if | PtxInUpperBlocks slotId <- _ptxCond,
         Just depth <- mdepth,
         longAgo depth slotId -> do
             void $ casPtxCondition _ptxWallet _ptxTxId _ptxCond PtxPersisted
             logInfo $ sformat ("Transaction "%build%" got persistent") _ptxTxId
  where
     longAgo depth (flattenSlotId -> ptxSlot) =
         ptxSlot + getBlockCount depth < flattenSlotId curSlot

-- | 'True' for slots which are equal to
-- @ptxCreationSlot + initialDelay + furtherDelay * k@ for some integer @k@
whetherCheckPtxOnSlot :: HasCoreConstants => SlotId -> PendingTx -> Bool
whetherCheckPtxOnSlot (flattenSlotId -> curSlot) ptx = do
    let ptxSlot = flattenSlotId (_ptxCreationSlot ptx)
        checkStartSlot = ptxSlot + initialDelay
    and [ curSlot > checkStartSlot
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
            -- these errors are likely caused by networking
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

-- | Checks number of remaining resubmision attempts.
-- Returns 'True' for still living transaction.
processPtxRemAttempts :: MonadPendings m => PendingTx -> m Bool
processPtxRemAttempts PendingTx{..} = do
    let expired = _ptxAttemptsRem <= 0
    when expired $ do
        logInfo $ sformat ("Pending transaction "%build%" has expired") _ptxTxId
        setPtxCondition _ptxWallet _ptxTxId (PtxWontApply "Exceeded resubmission attempts limit")
    return (not expired)

-- | Checks and updates state of given pending transactions, resubmitting them
-- if needed.
processPtxs
    :: MonadPendings m
    => SendActions m -> SlotId -> [PendingTx] -> m ()
processPtxs sendActions curSlot ptxs = do
    mapM_ (processPtxInUpperBlocks curSlot) ptxs

    livingPtxs <- filterM processPtxRemAttempts ptxs

    let toResubmit = filter ((PtxApplying ==) . _ptxCond) livingPtxs
    logInfo $ sformat fmt (map _ptxTxId toResubmit)
    resubmitPtxsDuringSlot sendActions toResubmit
  where
    fmt = "Transactions to resubmit on current slot: "%listJson

processPtxsOnSlot
    :: MonadPendings m
    => SendActions m -> SlotId -> m ()
processPtxsOnSlot sendActions curSlot = do
    ptxs <- getPendingTxs
    ptxsPerSlotLimit <- evalPtxsPerSlotLimit
    let selectedPtxs =
            take ptxsPerSlotLimit $
            sortWith _ptxCreationSlot $
            flip fromMaybe =<< topsortTxs wHash $
            filter (whetherCheckPtxOnSlot curSlot) $
            ptxs

    processPtxs sendActions curSlot selectedPtxs
  where
    wHash PendingTx{..} = WithHash (taTx _ptxTxAux) _ptxTxId
    evalPtxsPerSlotLimit = do
        slotDuration <- getLastKnownSlotDuration
        return $ fromIntegral $
            convertUnit slotDuration `div` pendingTxResubmitionPeriod

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
