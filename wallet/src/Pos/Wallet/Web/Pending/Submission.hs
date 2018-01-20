{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Transaction submission logic

module Pos.Wallet.Web.Pending.Submission
    ( PtxSubmissionHandlers (..)
    , ptxFirstSubmissionHandler
    , ptxResubmissionHandler

    , submitAndSavePtx
    ) where

import           Universum

import           Control.Monad.Catch          (onException)
import           Formatting                   (build, sformat, shown, stext, (%))
import           System.Wlog                  (WithLogger, logDebug, logInfo, logWarning)
import           Serokell.Util                (hour)

import           Pos.Client.Txp.History       (saveTx, thTimestamp)
import           Pos.Communication            (EnqueueMsg, submitTxRaw)
import           Pos.Configuration            (walletTxCreationDisabled)
import           Pos.Core                     (getCurrentTimestamp, diffTimestamp)
import           Pos.Util.Util                (maybeThrow)
import           Pos.Wallet.Web.Error         (WalletError (..))
import           Pos.Wallet.Web.Mode          (MonadWalletWebMode)
import           Pos.Wallet.Web.Pending.Types (PendingTx (..), PtxCondition (..),
                                               PtxPoolInfo)
import           Pos.Wallet.Web.Pending.Util  (isReclaimableFailure, ptxPoolInfo,
                                               usingPtxCoords)
import           Pos.Wallet.Web.State         (PtxMetaUpdate (PtxMarkAcknowledged),
                                               addOnlyNewPendingTx, casPtxCondition,
                                               ptxUpdateMeta, removeOnlyCreatingPtx)

-- | Handers used for to procees various pending transaction submission
-- errors.
-- If error is fatal for transaction, handler is supposed to throw exception.
data PtxSubmissionHandlers m = PtxSubmissionHandlers
    { -- | When fatal 'ToilVerFailure' occurs.
      -- Exception is not specified explicitely to prevent a wish
      -- to disassemble the cases - it's already done.
      pshOnNonReclaimable  :: forall e. (Exception e, Buildable e)
                           => e -> m ()
      -- | When minor error occurs, which means that transaction has
      -- a chance to be applied later.
    , pshOnMinor           :: SomeException -> m ()
    }

ptxFirstSubmissionHandler
    :: (MonadThrow m, WithLogger m)
    => PtxSubmissionHandlers m
ptxFirstSubmissionHandler =
    PtxSubmissionHandlers
    { pshOnNonReclaimable = throwM
    , pshOnMinor = \_ -> pass
    }

ptxResubmissionHandler
    :: MonadWalletWebMode m
    => PendingTx -> PtxSubmissionHandlers m
ptxResubmissionHandler PendingTx{..} =
    PtxSubmissionHandlers
    { pshOnNonReclaimable = \e ->
        if | _ptxPeerAck ->
             reportPeerAppliedEarlier
           | PtxApplying poolInfo <- _ptxCond -> do
             cancelPtx poolInfo e
             throwM e
           | otherwise ->
             reportBadCondition
    , pshOnMinor = \_ -> pass
    }
  where
    cancelPtx
        :: (MonadWalletWebMode m, Exception e, Buildable e)
        => PtxPoolInfo -> e -> m ()
    cancelPtx poolInfo e = do
        let newCond = PtxWontApply (sformat build e) poolInfo
        void $ casPtxCondition _ptxWallet _ptxTxId _ptxCond newCond
        reportCanceled

    reportPeerAppliedEarlier =
        logInfo $
        sformat ("Some peer applied tx #"%build%" earlier - continuing \
            \tracking")
            _ptxTxId
    reportCanceled =
        logInfo $
        sformat ("Pending transaction #"%build%" was canceled")
            _ptxTxId
    reportBadCondition =
        logWarning $
        sformat ("Processing failure of "%build%" resubmission, but \
            \this transaction has unexpected condition "%build)
            _ptxTxId _ptxCond


-- | Like 'Pos.Communication.Tx.submitAndSaveTx',
-- but treats tx as future /pending/ transaction.
submitAndSavePtx
    :: MonadWalletWebMode m
    => PtxSubmissionHandlers m -> EnqueueMsg m -> PendingTx -> m ()
submitAndSavePtx PtxSubmissionHandlers{..} enqueue ptx@PendingTx{..} = do
    -- this should've been checked before, but just in case
    when walletTxCreationDisabled $
        throwM $ InternalError "Transaction creation is disabled by configuration!"

    now <- getCurrentTimestamp
    if | PtxApplying poolInfo <- _ptxCond,
         Just creationTime <- poolInfo ^. thTimestamp,
         diffTimestamp now creationTime > hour 1 -> do
           let newCond = PtxWontApply "1h limit exceeded" poolInfo
           void $ casPtxCondition _ptxWallet _ptxTxId _ptxCond newCond
           logInfo $
             sformat ("Pending transaction #"%build%" discarded becauce \
                      \the 1h time limit was exceeded")
                      _ptxTxId
       | otherwise -> do
           (saveTx (_ptxTxId, _ptxTxAux)
               `catch` invalidTxHandler)
               `onException` creationFailedHandler  -- NB. 'onException' will
                                                    -- rethrow after the handler
                                                    -- finishes
           addOnlyNewPendingTx ptx
           ack <- submitTxRaw enqueue _ptxTxAux
           reportSubmitted ack

           poolInfo <- badInitPtxCondition `maybeThrow` ptxPoolInfo _ptxCond
           _ <- usingPtxCoords casPtxCondition ptx _ptxCond (PtxApplying poolInfo)
           when ack $ ptxUpdateMeta _ptxWallet _ptxTxId PtxMarkAcknowledged
  where
    invalidTxHandler e = if isReclaimableFailure e
        then minorError "reclaimable" (SomeException e)
        else nonReclaimableError (SomeException e)

    creationFailedHandler =
        -- tx creation shouldn't fail if any of peers accepted our tx, but still,
        -- if transaction was detected in blocks and its state got updated by tracker
        -- while transaction creation failed, due to protocol error or bug,
        -- then we better not remove this pending transaction
        void $ usingPtxCoords removeOnlyCreatingPtx ptx

    minorError desc e = do
        reportError desc e ", but was given another chance"
        pshOnMinor e
    nonReclaimableError e = do
        reportError "fatal" e ""
        pshOnNonReclaimable e

    reportError desc e outcome =
        logInfo $
        sformat ("Transaction #"%build%" application failed ("%shown%" - "
                %stext%")"%stext) _ptxTxId e desc outcome
    reportSubmitted ack =
        logDebug $
        sformat ("submitAndSavePtx: transaction submitted with confirmation?: "
                %build) ack

    badInitPtxCondition = InternalError "Expected PtxCreating as initial pending condition"
