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
import           System.Wlog                  (WithLogger, logInfo, logWarning)

import           Pos.Client.Txp.History       (saveTx)
import           Pos.Communication            (EnqueueMsg, submitTxRaw)
import           Pos.Wallet.Web.Mode          (MonadWalletWebMode)
import           Pos.Wallet.Web.Pending.Types (PendingTx (..), PtxCondition (..),
                                               PtxPoolInfo)
import           Pos.Wallet.Web.Pending.Util  (isReclaimableFailure)
import           Pos.Wallet.Web.State         (PtxMetaUpdate (PtxMarkAcknowledged),
                                               casPtxCondition, ptxUpdateMeta)


data PtxSubmissionHandlers m = PtxSubmissionHandlers
    { -- | When fatal 'ToilVerFailure' occurs.
      -- Exception is not specified explicitely to prevent a wish
      -- to disassemble the cases - it's already done.
      pshOnNonReclaimable  :: forall e. (Exception e, Buildable e)
                           => Bool -> e -> m ()
      -- | When minor error occurs, which means that transaction has
      -- a chance to beapplied later.
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
        if | peerAck ->
             reportPeerApplied
           | PtxApplying poolInfo <- _ptxCond ->
             cancelPtx poolInfo e
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

    reportPeerApplied =
        logInfo $
        sformat ("Peer applied tx "%build%", while we didn't - continuing \
            \tracking")
            _ptxTxId
    reportCanceled =
        logInfo $
        sformat ("Pending transaction "%build%" was canceled")
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
submitAndSavePtx PtxSubmissionHandlers{..} enqueue PendingTx{..} = do
    ack <- submitTxRaw enqueue _ptxTxAux
    when ack $ ptxUpdateMeta _ptxWallet _ptxTxId PtxMarkAcknowledged
    saveTx (_ptxTxId, _ptxTxAux) `catches` handlers ack
  where
    handlers accepted =
        [ Handler $ \e ->
            if isReclaimableFailure e
                then minorError "reclaimable" (SomeException e)
                else pshOnNonReclaimable accepted e

        , Handler $ \e@SomeException{} ->
            -- I don't know where this error can came from,
            -- but it's better to try with tx again than to regret, right?
            minorError "unknown error" e
        ]
    minorError desc e = do
        pshOnMinor e
        logInfo $
            sformat ("Transaction application failed ("%shown%" - "%stext%
                     "), but was given another chance") e desc

