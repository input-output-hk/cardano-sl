{-# LANGUAGE TypeFamilies #-}

-- | Pending transactions resubmition logic.

module Pos.Wallet.Web.Pending
    ( startPendingTxsResubmitter
    ) where

import           Universum

import           Data.Time.Units      (Second, convertUnit)
import           Formatting           (build, sformat)
import           Mockable             (delay, fork)

import           Pos.Communication    (SendActions)
import           Pos.Communication.Tx (submitAndSaveTx)
import           Pos.Core             (SlotId, getSlotCount)
import           Pos.Core.Slotting    (flattenSlotId)
import           Pos.Crypto           (WithHash (..))
import           Pos.Discovery.Class  (getPeers)
import           Pos.Slotting         (getLastKnownSlotDuration, onNewSlot)
import           Pos.Txp              (PendingTx (..), PtxCondition (..),
                                       ToilVerFailure (..), TxAux (..), getMemPool,
                                       processTx, ptxCreationSlot, runDBToil,
                                       runToilTLocal, topsortTxs)
import qualified Pos.Wallet.Web.Mode
import           Pos.Wallet.Web.State (getPendingTxs, setPtxCondition)

type MonadPendings m = m ~ Pos.Wallet.Web.Mode.WalletWebMode

type ToResubmit x = x

-- | Checks whether transaction is applicable.
-- Making it to work with all prepared pending transactions
-- is crucial due to chain transactions. TODO: complete. Maybe this have to go to resubmitter
canSubmitPtx
    :: MonadPendings m
    => [PendingTx] -> m (ToResubmit [PendingTx])
canSubmitPtx ptxs = do
    -- FIXME [CSM-256] do under blk semaphore!
    mp <- getMemPool
    runDBToil . fmap fst . runToilTLocal mempty mp mempty $
        concatForM ptxs $ \ptx@PendingTx{..} -> do
            res <- runExceptT $ processTx (ptxTxId, ptxTxAux)
            case res of
                Left e  -> lift . lift $ processFailure ptx e $> []
                Right _ -> return [ptx]
  where
    -- | What should happen with pending transaction if attempt
    -- to resubmit it failed.
    -- If number of 'ToilVerFailure' constructors will ever change, compiler
    -- will complain - for this purpose we consider all cases here.
    processFailure ptx e = do
        let await   = return ()
            discard = setPtxCondition ptx (PtxWon'tApply $ sformat build e)
        case e of
            ToilKnown                -> await
            ToilTipsMismatch{}       -> await
            ToilSlotUnknown          -> await
            ToilOverwhelmed{}        -> await
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
    canSubmitPtx $ filter ((PtxApplying ==) . ptxCond) ptxs
   where
     isPersistent ptx =
         flattenSlotId (ptxCreationSlot ptx) + getSlotCount undefined
             < flattenSlotId curSlot
     markPersistent ptx =
         when (ptxCond ptx == PtxInUpperBlocks && isPersistent ptx) $
             setPtxCondition ptx PtxPersisted

whetherCheckPtxOnSlot :: SlotId -> PendingTx -> Bool
whetherCheckPtxOnSlot curSlot ptx =
    -- TODO [CSM-256]: move 3 in constants?
    flattenSlotId (ptxCreationSlot ptx) + 3 < flattenSlotId curSlot

-- | On each slot this takes several pending transactions (using
-- 'whetherToResubmitPtx' rule) and resubmits them if needed & possible.
-- It's not /worker/, because it requires some constraints, natural only for
-- wallet environment.
startPendingTxsResubmitter
    :: MonadPendings m
    => SendActions m -> m ()
startPendingTxsResubmitter sendActions =
    onNewSlot False $ \curSlot -> do
        ptxs <- getPendingTxs
        let ptxsToCheck =
                flip fromMaybe =<< topsortTxs wHash $
                filter (whetherCheckPtxOnSlot curSlot) $
                ptxs
        -- FIXME [CSM-256]: add limit on number of resent transactions per slot or on speed?
        toResubmit <- processPtxs curSlot ptxsToCheck

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

