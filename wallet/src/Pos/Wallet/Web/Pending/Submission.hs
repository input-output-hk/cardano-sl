{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Transaction submission logic

module Pos.Wallet.Web.Pending.Submission
    ( PtxSubmissionHandlers (..)
    , ptxFirstSubmissionHandler
    , ptxResubmissionHandler

    , TxSubmissionMode
    , submitAndSavePtx
    ) where

import           Universum

import           Control.Monad.Catch (Handler (..), catches, onException)
import           Formatting (build, sformat, shown, stext, (%))
import           System.Wlog (WithLogger, logInfo)

import           Pos.Client.Txp.History (saveTx)
import           Pos.Client.Txp.Network (TxMode)
import           Pos.Core.Txp (TxAux)
import           Pos.Util.LogSafe (logInfoS, logWarningS)
import           Pos.Util.Util (maybeThrow)
import           Pos.Wallet.Web.Error (WalletError (InternalError))
import           Pos.Wallet.Web.Pending.Functions (isReclaimableFailure, ptxPoolInfo,
                                                   usingPtxCoords)
import           Pos.Wallet.Web.Pending.Types (PendingTx (..), PtxCondition (..), PtxPoolInfo)
import           Pos.Wallet.Web.State (MonadWalletDB, PtxMetaUpdate (PtxMarkAcknowledged),
                                       addOnlyNewPendingTx, casPtxCondition, ptxUpdateMeta,
                                       removeOnlyCreatingPtx)

-- | Handers used for to procees various pending transaction submission
-- errors.
-- If error is fatal for transaction, handler is supposed to rethrow exception.
data PtxSubmissionHandlers m = PtxSubmissionHandlers
    { -- | When fatal case of 'ToilVerFailure' occurs.
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
    :: forall ctx m. (MonadThrow m, WithLogger m, MonadWalletDB ctx m)
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
        :: (Exception e, Buildable e)
        => PtxPoolInfo -> e -> m ()
    cancelPtx poolInfo e = do
        let newCond = PtxWontApply (sformat build e) poolInfo
        void $ casPtxCondition _ptxWallet _ptxTxId _ptxCond newCond
        reportCanceled

    reportPeerAppliedEarlier =
        logInfoS $
        sformat ("Some peer applied tx #"%build%" earlier - continuing \
            \tracking")
            _ptxTxId
    reportPeerApplied =
        logInfoS $
        sformat ("Peer applied tx #"%build%", while we didn't - continuing \
            \tracking")
            _ptxTxId
    reportCanceled =
        logInfoS $
        sformat ("Pending transaction #"%build%" was canceled")
            _ptxTxId
    reportBadCondition =
        logWarningS $
        sformat ("Processing failure of "%build%" resubmission, but \
            \this transaction has unexpected condition "%build)
            _ptxTxId _ptxCond

type TxSubmissionMode ctx m =
    ( TxMode m
    , MonadWalletDB ctx m
    )

-- | Like 'Pos.Communication.Tx.submitAndSaveTx',
-- but treats tx as future /pending/ transaction.
submitAndSavePtx
    :: TxSubmissionMode ctx m
    => (TxAux -> m Bool)
    -> PtxSubmissionHandlers m
    -> PendingTx
    -> m ()
submitAndSavePtx submitTx PtxSubmissionHandlers{..} ptx@PendingTx{..} = do
    addOnlyNewPendingTx ptx
    ack <- submitTx _ptxTxAux
    (saveTx (_ptxTxId, _ptxTxAux)
        `catches` handlers ack)
        `onException` creationFailedHandler

    poolInfo <- badInitPtxCondition `maybeThrow` ptxPoolInfo _ptxCond
    _ <- usingPtxCoords casPtxCondition ptx _ptxCond (PtxApplying poolInfo)
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
        logInfoS $
        sformat ("Transaction #"%build%" application failed ("%shown%" - "
                %stext%")"%stext) _ptxTxId e desc outcome

    creationFailedHandler =
        -- tx creation shouldn't fail if any of peers accepted our tx, but still,
        -- if transaction was detected in blocks and its state got updated by tracker
        -- while transaction creation failed, due to protocol error or bug,
        -- then we better not remove this pending transaction
        void $ usingPtxCoords removeOnlyCreatingPtx ptx
    badInitPtxCondition = InternalError "Expected PtxCreating as initial pending condition"
