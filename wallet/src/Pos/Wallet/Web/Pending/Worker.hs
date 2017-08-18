{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Pending transactions resubmition logic.

module Pos.Wallet.Web.Pending.Worker
    ( MonadPendings
    , processPtxFailure
    , startPendingTxsResubmitter
    ) where

import           Universum

import           Control.Monad.Catch          (Handler (..), catches, handleAll)
import           Data.Time.Units              (Second, convertUnit)
import           Formatting                   (build, sformat, shown, stext, (%))
import           Mockable                     (delay, fork)
import           Serokell.Util.Text           (listJson)
import           System.Wlog                  (logError, logInfo, modifyLoggerName)

import           Pos.Client.Txp.Addresses     (MonadAddresses)
import           Pos.Communication            (submitAndSaveTx)
import           Pos.Communication.Protocol   (SendActions (..))
import           Pos.Constants                (pendingTxResubmitionPeriod)
import           Pos.Core                     (FlatSlotId, SlotId (..), getSlotCount)
import           Pos.Core.Context             (HasCoreConstants)
import           Pos.Core.Slotting            (flattenSlotId)
import           Pos.Crypto                   (WithHash (..))
import           Pos.Slotting                 (getLastKnownSlotDuration, onNewSlot)
import           Pos.Txp                      (ToilVerFailure (..), TxAux (..),
                                               topsortTxs)
import           Pos.Wallet.Web.Mode          (MonadWalletWebMode)
import           Pos.Wallet.Web.Pending.Types (PendingTx (..), PtxCondition (..))
import           Pos.Wallet.Web.State         (addOnlyNewPendingTx, casPtxCondition,
                                               getPendingTxs)

type MonadPendings m =
    ( MonadWalletWebMode m
    , MonadAddresses m
    , HasCoreConstants
    )

-- | What should happen with pending transaction if attempt
-- to resubmit it failed.
processPtxFailure :: MonadPendings m => Bool -> PendingTx -> ToilVerFailure -> m ()
processPtxFailure existing ptx@PendingTx{..} e =
    -- If number of 'ToilVerFailure' constructors will ever change, compiler
    -- will complain - for this purpose we consider all cases explicitly here.
    case e of
        ToilKnown               -> trackFurther
        ToilTipsMismatch{}      -> trackFurther
        ToilSlotUnknown         -> trackFurther
        ToilOverwhelmed{}       -> trackFurther
        ToilNotUnspent{}        -> discard
        ToilOutGTIn{}           -> discard
        ToilInconsistentTxAux{} -> discard
        ToilInvalidOutputs{}    -> discard
        ToilInvalidInputs{}     -> discard
        ToilTooLargeTx{}        -> discard
        ToilInvalidMinFee{}     -> discard
        ToilInsufficientFee{}   -> discard
        ToilUnknownAttributes{} -> discard
        ToilBootInappropriate{} -> discard
  where
    trackFurther
        | existing  = pass
        | otherwise = addOnlyNewPendingTx ptx
    discard
        | existing  = do
            let newCond = PtxWontApply (sformat build e)
            void $ casPtxCondition ptxTxId PtxApplying newCond
            logInfo $ sformat ("Transaction "%build%" was canceled") ptxTxId
        | otherwise = pass

processPtxInUpperBlocks :: MonadPendings m => SlotId -> PendingTx -> m ()
processPtxInUpperBlocks curSlot PendingTx{..}
    | PtxInUpperBlocks (slotId, _) <- ptxCond, longAgo slotId = do
         void $ casPtxCondition ptxTxId ptxCond PtxPersisted
         logInfo $ sformat ("Transaction "%build%" got persistent") ptxTxId
    | otherwise = pass
  where
     longAgo (flattenSlotId -> ptxSlot) =
         ptxSlot + getSlotCount ptxAssuredDepth < flattenSlotId curSlot

-- | 'True' for slots which are equal to
-- @ptxCreationSlot + initialDelay + furtherDelay * k@ for some integer @k@
whetherCheckPtxOnSlot :: HasCoreConstants => SlotId -> PendingTx -> Bool
whetherCheckPtxOnSlot (flattenSlotId -> curSlot) ptx = do
    let ptxSlot = flattenSlotId (ptxCreationSlot ptx)
        checkStartSlot = ptxSlot + initialDelay
    and [ curSlot > checkStartSlot
        , ((curSlot - checkStartSlot) `mod` furtherDelay) == 0
        ]
  where
    initialDelay = 3 :: FlatSlotId
    furtherDelay = 1 :: FlatSlotId

resubmitTx :: MonadPendings m => SendActions m -> PendingTx -> m ()
resubmitTx SendActions{..} ptx@PendingTx{..} = do
    logInfo $ sformat ("Resubmitting tx "%build) ptxTxId
    -- FIXME [CSM-256] Doesn't it introduce a race condition?
    void (submitAndSaveTx enqueueMsg ptxTxAux) `catches` handlers
  where
    handlers =
        [ Handler $ \e -> do
            reportFail "Failed to resubmit tx" e
            processPtxFailure True ptx e

        , Handler $ \(SomeException e) ->
            -- these errors are likely caused by networking
            reportFail "Failed to resubmit tx, will try again later" e
        ]
    reportFail :: MonadPendings m => Exception e => Text -> e -> m ()
    reportFail desc =
        logInfo . sformat (stext%" "%build%": "%shown) desc ptxTxId

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

-- | Checks and updates state of given pending transactions, resubmitting them
-- if needed.
processPtxs
    :: MonadPendings m
    => SendActions m -> SlotId -> [PendingTx] -> m ()
processPtxs sendActions curSlot ptxs = do
    mapM_ (processPtxInUpperBlocks curSlot) ptxs
    let toResubmit = filter ((PtxApplying ==) . ptxCond) ptxs
    logInfo $ sformat fmt (map ptxTxId toResubmit)
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
            sortWith ptxCreationSlot $
            flip fromMaybe =<< topsortTxs wHash $
            filter (whetherCheckPtxOnSlot curSlot) $
            ptxs

    processPtxs sendActions curSlot selectedPtxs
  where
    wHash PendingTx{..} = WithHash (taTx ptxTxAux) ptxTxId
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
    void . fork . setLogger . totalHandler $
    onNewSlot False (processPtxsOnSlot sa)
  where
    setLogger = modifyLoggerName (<> "tx" <> "resubmitter")
    totalHandler = handleAll $ logError . sformat ("Worker died: "%build)
