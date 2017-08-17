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
import           Pos.Wallet.Web.State         (casPtxCondition, getPendingTxs)

type MonadPendings m =
    ( m ~ Pos.Wallet.Web.Mode.WalletWebMode
    , MonadAddresses m  -- TODO [CSM-407]: for now there is no way to know
                        -- about this instance here
    )

-- | What should happen with pending transaction if attempt
-- to resubmit it failed.
processPtxFailure :: MonadPendings m => PendingTx -> ToilVerFailure -> m ()
processPtxFailure PendingTx{..} e =
    -- If number of 'ToilVerFailure' constructors will ever change, compiler
    -- will complain - for this purpose we consider all cases explicitly here.
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
  where
    tryLater = pass
    discard  = do
        casPtxCondition ptxTxId PtxApplying (PtxWon'tApply $ sformat build e)
        logInfo $ sformat ("Transaction "%build%" was canceled") ptxTxId

filterApplicablePtxs
    :: MonadPendings m
    => SlotId -> [PendingTx] -> m [PendingTx]
filterApplicablePtxs curSlot ptxs = do
    mp <- getMemPool
    runDBToil . fmap fst . runToilTLocal mempty mp mempty $
        concatForM ptxs $ \ptx@PendingTx{..} -> do
            res <- runExceptT $ processTx (siEpoch curSlot) (ptxTxId, ptxTxAux)
            case res of
                Left e  -> lift . lift $ processPtxFailure ptx e $> []
                Right _ -> return [ptx]

processPtxInUpperBlocks :: MonadPendings m => SlotId -> PendingTx -> m ()
processPtxInUpperBlocks curSlot PendingTx{..}
    | PtxInUpperBlocks (slotId, _) <- ptxCond, longAgo slotId = do
         casPtxCondition ptxTxId ptxCond PtxPersisted
         logInfo $ sformat ("Transaction "%build%" got persistent") ptxTxId
    | otherwise = pass
  where
     longAgo (flattenSlotId -> ptxSlotId) = do
         ptxSlotId + getSlotCount ptxAssuredDepth < flattenSlotId curSlot

-- | 'True' for slots which are equal to
-- @ptxCreationSlot + initialDelay + furtherDelay * k@ for some integer @k@
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
    toResubmit <- filterApplicablePtxs curSlot $
                  filter ((PtxApplying ==) . ptxCond) ptxs
    logInfo $ sformat fmt (map ptxTxId toResubmit)
    resubmitPtxsDuringSlot sendActions ptxs
  where
    fmt = "Transactions to resubmit on current slot: "%listJson

processPtxsOnSlot
    :: MonadPendings m
    => SendActions m -> SlotId -> m ()
processPtxsOnSlot sendActions curSlot = do
    ptxs <- getPendingTxs
    ptxsPerSlotLimit <- evalPtxsPerSlotLimit
    let ptxsToProcess =
            take ptxsPerSlotLimit $
            sortWith ptxCreationSlot $
            flip fromMaybe =<< topsortTxs wHash $
            filter (whetherCheckPtxOnSlot curSlot) $
            ptxs

    processPtxs sendActions curSlot ptxsToProcess
  where
    wHash PendingTx{..} = WithHash (taTx ptxTxAux) ptxTxId
    evalPtxsPerSlotLimit = do
        slotDuration <- getLastKnownSlotDuration
        return $ fromIntegral $
            convertUnit slotDuration `div` ptxsResubmitionPeriod
    -- FIXME [CSM-256]: move to constants?
    ptxsResubmitionPeriod = 10 :: Second

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
