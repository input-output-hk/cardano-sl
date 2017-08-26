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
import           Pos.Communication            (EnqueueMsg, TxMode, submitTxRaw)
import           Pos.Crypto                   (hash)
import           Pos.Txp                      (TxAux (..))
import           Pos.Wallet.Web.Mode          (MonadWalletWebMode)
import           Pos.Wallet.Web.Pending.Types (PendingTx (..), PtxCondition (..),
                                               PtxPoolInfo)
import           Pos.Wallet.Web.Pending.Util  (isReclaimableFailure)
import           Pos.Wallet.Web.State         (casPtxCondition)


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
    :: (TxMode ssc ctx m, MonadCatch m)
    => PtxSubmissionHandlers m -> EnqueueMsg m -> TxAux -> m ()
submitAndSavePtx PtxSubmissionHandlers{..} enqueue txAux@TxAux{..} = do
    let txId = hash taTx
    accepted <- submitTxRaw enqueue txAux
    saveTx (txId, txAux) `catches` handlers accepted
  where
    handlers accepted =
        [ Handler $ \e ->
            if isReclaimableFailure e
                then minorError "reclaimable" (SomeException e)
                else pshOnNonReclaimable accepted e

        , Handler $ \e@SomeException{} ->
            -- I don't know where this error can came from,
            -- but it's better to try with tx again than to regret, right?
            -- At the moment of writting this no network error can be here.
            minorError "unknown error" e
        ]

    minorError desc e = do
        pshOnMinor e
        logInfo $
            sformat ("Transaction application failed ("%shown%" - "%stext%
                     "), but was given another chance") e desc

