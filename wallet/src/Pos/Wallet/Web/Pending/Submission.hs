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

import           Control.Monad.Catch          (Handler (..), catches)
import           Formatting                   (build, sformat, shown, stext, (%))
import           System.Wlog                  (WithLogger, logInfo)

import           Pos.Client.Txp.History       (saveTx)
import           Pos.Communication            (EnqueueMsg, submitTxRaw)
import           Pos.Util.LogSafe             (buildSafe, logInfoSP, logWarningSP,
                                               secretOnlyF)
import           Pos.Wallet.Web.Mode          (MonadWalletWebMode)
import           Pos.Wallet.Web.Pending.Types (PendingTx (..), PtxCondition (..),
                                               PtxPoolInfo)
import           Pos.Wallet.Web.Pending.Util  (isReclaimableFailure)
import           Pos.Wallet.Web.State         (PtxMetaUpdate (PtxMarkAcknowledged),
                                               addOnlyNewPendingTx, casPtxCondition,
                                               ptxUpdateMeta)

-- | Handers used for to procees various pending transaction submission
-- errors.
-- If error is fatal for transaction, handler is supposed to throw exception.
data PtxSubmissionHandlers m = PtxSubmissionHandlers
    { -- | When fatal 'ToilVerFailure' occurs.
      -- Exception is not specified explicitely to prevent a wish
      -- to disassemble the cases - it's already done.
      pshOnNonReclaimable  :: forall e. (Exception e, Buildable e)
                           => Bool -> e -> m ()
      -- | When minor error occurs, which means that transaction has
      -- a chance to be applied later.
    , pshOnMinor           :: SomeException -> m ()
    }

ptxFirstSubmissionHandler
    :: (MonadThrow m, WithLogger m)
    => PtxSubmissionHandlers m
ptxFirstSubmissionHandler =
    PtxSubmissionHandlers
    { pshOnNonReclaimable = \peerAck e ->
        if peerAck
        then reportPeerApplied
        else throwM e
    , pshOnMinor = \_ -> pass
    }
  where
     reportPeerApplied =
        logInfo "Some peer applied new tx, while we didn't - considering \
                \transaction made"

ptxResubmissionHandler
    :: MonadWalletWebMode m
    => PendingTx -> PtxSubmissionHandlers m
ptxResubmissionHandler PendingTx{..} =
    PtxSubmissionHandlers
    { pshOnNonReclaimable = \peerAck e ->
        if | _ptxPeerAck ->
             reportPeerAppliedEarlier
           | peerAck ->
             reportPeerApplied
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
        logInfoSP $ \sl ->
        sformat ("Some peer applied tx #"%secretOnlyF sl build%" earlier - continuing \
            \tracking")
            _ptxTxId
    reportPeerApplied =
        logInfoSP $ \sl ->
        sformat ("Peer applied tx #"%secretOnlyF sl build%", while we didn't - continuing \
            \tracking")
            _ptxTxId
    reportCanceled =
        logInfoSP $ \sl ->
        sformat ("Pending transaction #"%secretOnlyF sl build%" was canceled")
            _ptxTxId
    reportBadCondition =
        logWarningSP $ \sl ->
        sformat ("Processing failure of "%secretOnlyF sl build%" resubmission, but \
            \this transaction has unexpected condition "%buildSafe sl)
            _ptxTxId _ptxCond

-- | Like 'Pos.Communication.Tx.submitAndSaveTx',
-- but treats tx as future /pending/ transaction.
submitAndSavePtx
    :: MonadWalletWebMode m
    => PtxSubmissionHandlers m -> EnqueueMsg m -> PendingTx -> m ()
submitAndSavePtx PtxSubmissionHandlers{..} enqueue ptx@PendingTx{..} = do
    ack <- submitTxRaw enqueue _ptxTxAux
    saveTx (_ptxTxId, _ptxTxAux) `catches` handlers ack
    addOnlyNewPendingTx ptx
    when ack $ ptxUpdateMeta _ptxWallet _ptxTxId PtxMarkAcknowledged
  where
    handlers accepted =
        [ Handler $ \e ->
            if isReclaimableFailure e
                then minorError "reclaimable" (SomeException e)
                else nonReclaimableError accepted (SomeException e)

        , Handler $ \e@SomeException{} ->
            -- I don't know where this error can came from,
            -- but it's better to try with tx again than to regret, right?
            minorError "unknown error" e
        ]

    minorError desc e = do
        reportError desc e ", but was given another chance"
        pshOnMinor e
    nonReclaimableError accepted e = do
        reportError "fatal" e ""
        pshOnNonReclaimable accepted e

    reportError desc e outcome =
        logInfoSP $ \sl ->
        sformat ("Transaction #"%secretOnlyF sl build%" application failed ("%shown%" - "
                %stext%")"%stext) _ptxTxId e desc outcome
