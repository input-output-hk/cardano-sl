-- | Pending transactions resubmition logic.

module Pos.Wallet.Web.Pending
    ( startPendingTxsResubmitter
    ) where

import           Universum

import           Data.Time.Units         (Second, convertUnit)
import           Formatting              (build, sformat)
import           Mockable                (delay, fork)

import           Pos.Client.Txp.Balances (MonadBalances)
import           Pos.Client.Txp.History  (MonadTxHistory)
import           Pos.Communication       (SendActions)
import           Pos.Communication.Tx    (submitAndSaveTx)
import           Pos.Core                (SlotId, getSlotCount)
import           Pos.Core.Slotting       (flattenSlotId)
import           Pos.Crypto              (WithHash (..))
import           Pos.Discovery.Class     (getPeers)
import           Pos.Slotting            (getLastKnownSlotDuration, onNewSlot)
import           Pos.Txp.Core            (TxAux (..), topsortTxs)
import           Pos.Txp.MemState        (getMemPool)
import           Pos.Txp.Pending         (PendingTx (..), PtxCondition (..),
                                          isPtxInBlocks, ptxCreationSlot, ptxExpireSlot)
import           Pos.Txp.Toil            (ToilVerFailure (..), execToilTLocal, processTx,
                                          runDBToil)
import           Pos.WorkMode.Class      (WorkMode)

type MonadPendings ssc ctx m =
    ( WorkMode ssc ctx m
    , MonadTxHistory ssc m
    , MonadBalances m
    )

-- | 'Left' stands for stable pending transaction state,
-- 'Right' means that transaction have to be resubmitted.
type ResubmitCond = Bool

setPtxCondition :: PendingTx -> PtxCondition -> m ()
setPtxCondition = undefined

getPtxCondition :: PendingTx -> m PtxCondition
getPtxCondition = undefined

getPendingTxs :: PtxCondition -> m [PendingTx]
getPendingTxs = undefined

canSubmitPtx
    :: MonadPendings ssc ctx m
    => PendingTx -> m ResubmitCond
canSubmitPtx ptx@PendingTx{..} = do
    -- FIXME [CSM-256] do under blk semaphore!
    mp <- getMemPool
    res <- runExceptT . runDBToil . execToilTLocal mempty mp mempty $
        processTx (ptxTxId, ptxTxAux)
    either processFailure return $ res $> True
  where
    await     = return False
    discard e = False <$ setPtxCondition ptx (PtxWon'tApply $ sformat build e)

    -- | What should happen with pending transaction if attempt
    -- to resubmit it failed.
    processFailure e = case e of
        ToilKnown                -> await
        ToilTipsMismatch{}       -> await
        ToilSlotUnknown          -> await
        ToilOverwhelmed{}        -> await
        ToilNotUnspent{}         -> discard e
        ToilOutGTIn{}            -> discard e
        ToilInconsistentTxAux{}  -> discard e
        ToilInvalidOutputs{}     -> discard e
        ToilInvalidInputs{}      -> discard e
        ToilTooLargeTx{}         -> discard e
        ToilInvalidMinFee{}      -> discard e
        ToilInsufficientFee{}    -> discard e
        ToilUnknownAttributes{}  -> discard e
        ToilBootDifferentStake{} -> discard e

processPtx
    :: MonadPendings ssc ctx m
    => SlotId -> PendingTx -> m ResubmitCond
processPtx curSlot ptx = do
    cond <- getPtxCondition ptx
    if | and
         [ cond == PtxInUpperBlocks
         , flattenSlotId (ptxCreationSlot ptx) + getSlotCount undefined
             < flattenSlotId curSlot
         ] ->
           False <$ setPtxCondition ptx PtxPersisted
       | cond == PtxApplying ->
           canSubmitPtx ptx
       | otherwise ->
           return False

whetherCheckPtxOnSlot :: SlotId -> PendingTx -> Bool
whetherCheckPtxOnSlot curSlot ptx =
    -- TODO [CSM-256]: move 3 in constants?
    flattenSlotId (ptxCreationSlot ptx) + 3 < flattenSlotId curSlot

-- | On each slot this takes several pending transactions (using
-- 'whetherToResubmitPtx' rule) and resubmits them if needed & possible.
-- It's not /worker/, because it requires some constraints, natural only for
-- wallet environment.
startPendingTxsResubmitter
    :: MonadPendings ssc ctx m
    => SendActions m -> m ()
startPendingTxsResubmitter sendActions =
    onNewSlot False $ \curSlot -> do
        ptxs <- getPendingTxs PtxApplying
        let ptxsToCheck =
                flip fromMaybe =<< topsortTxs wHash $
                filter (whetherCheckPtxOnSlot curSlot) $
                ptxs
        -- FIXME [CSM-256]: add limit on number of resent transactions per slot or on speed?
        toResubmit <- filterM (processPtx curSlot) ptxsToCheck

        -- distribute txs submition over current slot ~evenly
        interval <- evalSubmitDelay (length toResubmit)
        na <- toList <$> getPeers
        forM_ toResubmit $ \PendingTx{..} -> do
            delay interval
            -- FIXME [CSM-256] Doesn't it introduce a race condition?
            fork $ submitAndSaveTx sendActions na ptxTxAux
  where
    wHash PendingTx{..} = WithHash (taTx ptxTxAux) ptxTxId
    submitionEta = 5 :: Second
    evalSubmitDelay toResubmitNum = do
        slotDuration <- getLastKnownSlotDuration
        let checkPeriod = max 0 $ slotDuration - convertUnit submitionEta
        return (checkPeriod `div` fromIntegral toResubmitNum)

