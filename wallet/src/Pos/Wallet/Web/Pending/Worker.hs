{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Pending transactions resubmition logic.

module Pos.Wallet.Web.Pending.Worker
    ( startPendingTxsResubmitter
    ) where

import           Universum

import           Control.Monad.Catch          (handleAll)
import           Data.Time.Units              (Second, convertUnit)
import           Formatting                   (build, sformat, shown, (%))
import           Mockable                     (delay, fork)
import           Serokell.Util.Text           (listJson)
import           System.Wlog                  (logError, logInfo, modifyLoggerName)

import           Pos.Client.Txp.Addresses     (MonadAddresses)
import           Pos.Communication            (submitAndSave)
import           Pos.Communication.Protocol   (SendActions (..))
import           Pos.Core                     (FlatSlotId, SlotId (..), getSlotCount)
import           Pos.Core.Slotting            (flattenSlotId)
import           Pos.Crypto                   (WithHash (..))
import           Pos.Slotting                 (getLastKnownSlotDuration, onNewSlot)
import           Pos.Txp                      (ToilVerFailure (..), TxAux (..),
                                               getMemPool, processTx, runDBToil,
                                               runToilTLocal, topsortTxs)
import qualified Pos.Wallet.Web.Mode
import           Pos.Wallet.Web.Pending.Types (PendingTx (..), PtxCondition (..))
import           Pos.Wallet.Web.State         (getPendingTxs, setPtxCondition)

type MonadPendings m =
    ( m ~ Pos.Wallet.Web.Mode.WalletWebMode
    , MonadAddresses m  -- TODO [CSM-407]: for now there is no way to know
                        -- about this instance here
    )

type ToResubmit x = x

-- | Checks whether transaction is applicable.
-- Making it to work with all prepared pending transactions
-- is crucial due to chain transactions. TODO: complete. Maybe this have to go to resubmitter
canSubmitPtx
    :: MonadPendings m
    => SlotId -> [PendingTx] -> m (ToResubmit [PendingTx])
canSubmitPtx curSlot ptxs = do
    -- FIXME [CSM-256] do under blk semaphore!
    mp <- getMemPool
    runDBToil . fmap fst . runToilTLocal mempty mp mempty $
        concatForM ptxs $ \ptx@PendingTx{..} -> do
            res <- runExceptT $ processTx (siEpoch curSlot) (ptxTxId, ptxTxAux)
            case res of
                Left e  -> lift . lift $ processFailure ptx e $> []
                Right _ -> return [ptx]
  where
    -- | What should happen with pending transaction if attempt
    -- to resubmit it failed.
    -- If number of 'ToilVerFailure' constructors will ever change, compiler
    -- will complain - for this purpose we consider all cases explicitly here.
    processFailure PendingTx{..} e = do
        let tryLater = pass
            discard  = do
                setPtxCondition ptxTxId (PtxWon'tApply $ sformat build e)
                logInfo $ sformat ("Transaction "%build%" was canceled")
                          ptxTxId

        case e of
            ToilKnown                -> tryLater
            ToilTipsMismatch{}       -> tryLater
            ToilSlotUnknown          -> tryLater
            ToilOverwhelmed{}        -> tryLater
            ToilNotUnspent{}         -> discard
            ToilOutGTIn{}            -> discard
            ToilInconsistentTxAux{}  -> discard
            ToilInvalidOutputs{}     -> discard
            ToilInvalidInputs{}      -> discard
            ToilTooLargeTx{}         -> discard
            ToilInvalidMinFee{}      -> discard
            ToilInsufficientFee{}    -> discard
            ToilUnknownAttributes{}  -> discard
            ToilBootDifferentStake{} -> discard

processPtxs
    :: MonadPendings m
    => SlotId -> [PendingTx] -> m (ToResubmit [PendingTx])
processPtxs curSlot ptxs = do
    mapM_ markPersistent ptxs
    canSubmitPtx curSlot $ filter ((PtxApplying ==) . ptxCond) ptxs
   where
     longAgo PendingTx{..} (flattenSlotId -> ptxSlotId) = do
         ptxSlotId + getSlotCount ptxAssuredDepth < flattenSlotId curSlot
     markPersistent ptx@PendingTx{..}
         | PtxInUpperBlocks slotId <- ptxCond, longAgo ptx slotId = do
             setPtxCondition ptxTxId PtxPersisted
             logInfo $ sformat ("Transaction "%build%" got persistent") ptxTxId
         | otherwise = pass

whetherCheckPtxOnSlot :: SlotId -> PendingTx -> Bool
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
resubmitTx SendActions{..} PendingTx{..} = do
    logInfo $ sformat ("Resubmitting tx "%build) ptxTxId
    -- FIXME [CSM-256] Doesn't it introduce a race condition?
    void (submitAndSave enqueueMsg ptxTxAux) `catchAll` handler
  where
    handler e = do
        -- TODO [CSM-256] consider errors
        logInfo $ sformat ("Failed to resubmit tx "%build%": "%shown) ptxTxId e

startPendingTxsResubmitterDo
    :: MonadPendings m
    => SendActions m -> SlotId -> m ()
startPendingTxsResubmitterDo sendActions curSlot = do
    ptxs <- getPendingTxs
    let ptxsToCheck =
            flip fromMaybe =<< topsortTxs wHash $
            filter (whetherCheckPtxOnSlot curSlot) $
            ptxs
    -- FIXME [CSM-256]: add limit on number of resent transactions per slot or on speed?
    toResubmit <- processPtxs curSlot ptxsToCheck
    logInfo $ sformat ("Transactions to resubmit on current slot: "%listJson)
              (map ptxTxId toResubmit)

    -- distribute txs submition over current slot ~evenly
    interval <- evalSubmitDelay (length toResubmit)
    forM_ toResubmit $ \ptx -> do
        delay interval
        fork $ resubmitTx sendActions ptx
  where
    wHash PendingTx{..} = WithHash (taTx ptxTxAux) ptxTxId
    submitionEta = 5 :: Second
    evalSubmitDelay toResubmitNum = do
        slotDuration <- getLastKnownSlotDuration
        let checkPeriod = max 0 $ slotDuration - convertUnit submitionEta
        return (checkPeriod `div` fromIntegral toResubmitNum)

-- | On each slot this takes several pending transactions (using
-- 'whetherToResubmitPtx' rule) and resubmits them if needed and possible.
startPendingTxsResubmitter
    :: MonadPendings m
    => SendActions m -> m ()
startPendingTxsResubmitter sa =
    void . fork . setLogger . totalHandler $
    onNewSlot False (startPendingTxsResubmitterDo sa)
  where
    setLogger = modifyLoggerName (<> "tx" <> "resubmitter")
    totalHandler = handleAll $ logError . sformat ("Worker died: "%build)
