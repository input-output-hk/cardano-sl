{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Txp.Worker
       ( txpWorkers
       ) where

import           Universum

import           Pos.Communication       (OutSpecs, WorkerSpec)
import           Pos.Ssc.Class           (SscWorkersClass)
import           Pos.Util                (mconcatPair)
import           Pos.WorkMode.Class      (WorkMode)

#ifdef WITH_WALLET
import           Data.Tagged             (Tagged (..))
import           Data.Time.Units         (Second, convertUnit)
import           Formatting              (build, int, sformat, shown, (%))
import           Mockable                (delay, fork)
import           Serokell.Util           (listJson)
import           System.Wlog             (logDebug, logInfo, logWarning)

import           Pos.Client.Txp.Balances (MonadBalances)
import           Pos.Client.Txp.History  (MonadTxHistory)
import           Pos.Communication       (Conversation (..), ConversationActions (..),
                                          DataMsg (..), InvMsg (..), InvOrData,
                                          InvReqDataParams (..), MempoolMsg (..), NodeId,
                                          ReqMsg (..), SendActions, TxMsgContents, convH,
                                          expectData, handleDataDo, handleInvDo,
                                          recvLimited, toOutSpecs, withConnectionTo,
                                          worker)
import           Pos.Communication.Tx    (submitAndSaveTx)
import           Pos.Core                (SlotId, getSlotCount)
import           Pos.Core.Slotting       (flattenSlotId)
import           Pos.Discovery.Class     (getPeers)
import Pos.Constants (slotSecurityParam)
import           Pos.Slotting            (getLastKnownSlotDuration, onNewSlot)
import           Pos.Txp.Core            (TxId)
import           Pos.Txp.Network         (txInvReqDataParams)
import           Pos.Txp.Pending         (MonadPendingTxs (..), PendingTx (..),
                                          TxPendingState (..), ptxCreationSlot,
                                          ptxExpireSlot)
#endif

-- | All workers specific to transaction processing.
txpWorkers
    :: (SscWorkersClass ssc, MonadPendings ssc m, WorkMode ssc ctx m)
    => ([WorkerSpec m], OutSpecs)
txpWorkers =
    merge $ []
#if defined(WITH_WALLET)
            ++ [ queryTxsWorker
               , resubmitPendingTxsWorker
               ]
#endif
  where
    merge = mconcatPair . map (first pure)

#if defined(WITH_WALLET)

-- | When we're behind NAT, other nodes can't send data to us and thus we
-- won't get transactions that are broadcast through the network – we have to
-- reach out to other nodes by ourselves.
--
-- This worker just triggers every @max (slotDur / 4) 5@ seconds and asks for
-- tx mempool.
queryTxsWorker
    :: (WorkMode ssc ctx m, SscWorkersClass ssc)
    => (WorkerSpec m, OutSpecs)
queryTxsWorker = worker queryTxsSpec $ \sendActions -> do
    slotDur <- getLastKnownSlotDuration
    nodesRef <- liftIO . newIORef . toList =<< getPeers
    let delayInterval = max (slotDur `div` 4) (convertUnit (5 :: Second))
        action = forever $ do
            -- If we ran out of nodes to query, refresh the list
            whenM (null <$> liftIO (readIORef nodesRef)) $
                liftIO . writeIORef nodesRef . toList =<< getPeers
            -- If we managed to get any nodes (or if the list wasn't empty in
            -- the first place), we ask the first node for the tx mempool.
            liftIO (readIORef nodesRef) >>= \case
                []          -> return ()
                (node:rest) -> do
                    liftIO $ writeIORef nodesRef rest
                    txs <- getTxMempoolInvs sendActions node
                    requestTxs sendActions node txs
            delay $ delayInterval
        handler (e :: SomeException) = do
            logWarning $ "Exception arised in queryTxsWorker: " <> show e
            delay $ delayInterval * 2
            action `catch` handler
    action `catch` handler

type TxIdT = Tagged TxMsgContents TxId

-- | A specification of how 'queryTxsWorker' will do communication:
--
--     * It will send a MempoolMsg
--
--     * It will receive some InvOrData messages (containing transactions
--       from the mempool)
queryTxsSpec :: OutSpecs
queryTxsSpec =
    toOutSpecs
        [ -- used by 'getTxMempoolInvs'
          convH (Proxy @(MempoolMsg TxMsgContents))
                (Proxy @(InvMsg TxIdT))
          -- used by 'requestTxs'
        , convH (Proxy @(ReqMsg TxIdT))
                (Proxy @(InvOrData TxIdT TxMsgContents))
        ]

-- | Send a MempoolMsg to a node and receive incoming 'InvMsg's with
-- transaction IDs.
getTxMempoolInvs
    :: WorkMode ssc ctx m
    => SendActions m -> NodeId -> m [TxId]
getTxMempoolInvs sendActions node = do
    logInfo ("Querying tx mempool from node " <> show node)
    withConnectionTo sendActions node $ \_ -> pure $ Conversation $
      \(conv :: (ConversationActions
                                (MempoolMsg TxMsgContents)
                                (InvMsg TxIdT)
                                m)
      ) -> do
          send conv MempoolMsg
          let getInvs = do
                inv' <- recvLimited conv
                case inv' of
                    Nothing -> return []
                    Just (InvMsg{..}) -> do
                        useful <-
                          case txInvReqDataParams of
                            InvReqDataParams {..} ->
                                handleInvDo handleInv imKey
                        case useful of
                            Nothing           -> getInvs
                            Just (Tagged key) -> (key:) <$> getInvs
          getInvs

-- | Request several transactions.
requestTxs
    :: WorkMode ssc ctx m
    => SendActions m -> NodeId -> [TxId] -> m ()
requestTxs sendActions node txIds = do
    logInfo $ sformat
        ("Requesting "%int%" txs from node "%shown)
        (length txIds) node
    logDebug $ sformat
        ("First 5 (or less) transactions: "%listJson)
        (take 5 txIds)
    withConnectionTo sendActions node $ \_ -> pure $ Conversation $
     \(conv :: (ConversationActions
                                (ReqMsg TxIdT)
                                (InvOrData TxIdT TxMsgContents)
                                m)
      ) -> do
          let getTx id = do
                  logDebug $ sformat ("Requesting transaction "%build) id
                  send conv $ ReqMsg id
                  dt' <- recvLimited conv
                  case dt' of
                      Nothing -> error "didn't get an answer to Req"
                      Just x  -> flip expectData x $
                        \(DataMsg dmContents) ->
                          case txInvReqDataParams of
                            InvReqDataParams {..} ->
                                handleDataDo contentsToKey handleData dmContents
          for_ txIds $ \(Tagged -> id) ->
              getTx id `catch` handler id
    logInfo $ sformat
        ("Finished requesting txs from node "%shown)
        node
  where
    handler id (e :: SomeException) = do
        logWarning $ sformat
            ("Couldn't get transaction with id "%build%" "%
             "from node "%build%": "%shown)
            id node e


type MonadPendings ssc m =
    ( MonadBalances m
    , MonadTxHistory ssc m
    )

resubmitPendingTxsWorker
    :: (WorkMode ssc ctx m, MonadPendings ssc m)
    => (WorkerSpec m, OutSpecs)
resubmitPendingTxsWorker = worker undefined $ \sendActions ->
    onNewSlot False $ \curSlot -> do
        pendingTxs <- getPendingTxs TxApplying
        let ptxsToCheck =
                filter (whetherCheckPtxOnSlot curSlot) pendingTxs
        -- FIXME [CSM-256]: add limit on number of resent transactions per slot or on speed
        toResubmit <- filterM (whetherToResubmitPtx curSlot) ptxsToCheck

        -- distribute txs submition over current slot ~evenly
        interval <- evalSubmitDelay (length toResubmit)
        na <- toList <$> getPeers
        forM_ toResubmit $ \PendingTx{..} -> do
            delay interval
            fork $ submitAndSaveTx sendActions na ptTxAux

  where
    whetherCheckPtxOnSlot curSlot ptx =
        -- TODO [CSM-256]: move in constants?
        flattenSlotId (ptxCreationSlot ptx) + 3 < flattenSlotId curSlot

    -- TODO [CSM-256]: move in constants?
    submitionEta = 5

    evalSubmitDelay toResubmitNum = do
        slotDuration <- getLastKnownSlotDuration
        let checkPeriod = max 0 $ slotDuration - submitionEta
        return (checkPeriod `div` fromIntegral toResubmitNum)


whetherToResubmitPtx
    :: (WorkMode ssc ctx m, MonadPendings ssc m)
    => SlotId -> PendingTx -> m Bool
whetherToResubmitPtx curSlot ptx = do
    inBlocks <- isPtxInRecentBlocks ptx
    if | and
       [ inBlocks
       , flattenSlotId (ptxCreationSlot ptx) + getSlotCount slotSecurityParam
           < flattenSlotId curSlot
       ] ->
           False <$ removePendingTx ptx
       | expired ->
           False <$ setPendingTx ptx TxWon'tSend
       | not inBlocks ->
           canSubmitPtx ptx
       | otherwise    ->
           return False
  where
    isPtxInRecentBlocks = undefined
    expired = ptxExpireSlot ptx <= curSlot

canSubmitPtx
    :: (WorkMode ssc ctx m, MonadPendings ssc m)
    => PendingTx -> m Bool
canSubmitPtx ptx@PendingTx{..} = do
    present   <- checkTxIsInMempool ptTxAux
    applyable <- checkTxIsApplicable ptTxAux
    if | present   -> return False
                      -- TODO [CSM-256]: maybe get rid of na param
       | applyable -> return True
       | otherwise -> False <$ setPendingTx ptx TxWon'tSend
  where
    checkTxIsInMempool = undefined
    checkTxIsApplicable = undefined

#endif
