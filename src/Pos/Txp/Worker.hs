{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Txp.Worker
       ( txpWorkers
       ) where

import           Universum

import           Pos.Communication   (OutSpecs, WorkerSpec)
import           Pos.Ssc.Class       (SscWorkersClass)
import           Pos.Util            (mconcatPair)
import           Pos.WorkMode.Class  (WorkMode)

#ifdef WITH_WALLET
import           Data.Tagged         (Tagged (..))
import           Data.Time.Units     (Second, convertUnit)
import           Formatting          (build, int, sformat, shown, (%))
import           Mockable            (delay)
import           Serokell.Util       (listJson)
import           System.Wlog         (logDebug, logInfo, logWarning)

import           Pos.Communication   (Conversation (..), ConversationActions (..),
                                      DataMsg (..), InvMsg (..), InvOrData,
                                      InvReqDataParams (..), MempoolMsg (..), NodeId,
                                      ReqMsg (..), SendActions, SmartLimit, TxMsgContents,
                                      convH, expectData, handleDataDo, handleInvDo,
                                      reifyMsgLimit, toOutSpecs, withConnectionTo,
                                      withLimitedLength, worker)
import           Pos.Discovery.Class (getPeers)
import           Pos.Slotting        (getLastKnownSlotDuration)
import           Pos.Txp.Core        (TxId)
import           Pos.Txp.Network     (txInvReqDataParams)
#endif

-- | All workers specific to transaction processing.
txpWorkers
    :: (SscWorkersClass ssc, WorkMode ssc m)
    => ([WorkerSpec m], OutSpecs)
txpWorkers =
    merge $ []
#if defined(WITH_WALLET)
            ++ [ queryTxsWorker ]
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
    :: (WorkMode ssc m, SscWorkersClass ssc)
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
    :: WorkMode ssc m
    => SendActions m -> NodeId -> m [TxId]
getTxMempoolInvs sendActions node = do
    logInfo ("Querying tx mempool from node " <> show node)
    reifyMsgLimit (Proxy @(InvMsg TxIdT)) $
      \(_ :: Proxy s) -> withConnectionTo sendActions node $ \_ -> pure $ Conversation $
        \(conv :: (ConversationActions
                                  (MempoolMsg TxMsgContents)
                                  (SmartLimit s (InvMsg TxIdT))
                                  m)
        ) -> do
            send conv MempoolMsg
            let getInvs = do
                  inv' <- recv conv
                  case withLimitedLength <$> inv' of
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
    :: WorkMode ssc m
    => SendActions m -> NodeId -> [TxId] -> m ()
requestTxs sendActions node txIds = do
    logInfo $ sformat
        ("Requesting "%int%" txs from node "%shown)
        (length txIds) node
    logDebug $ sformat
        ("First 5 (or less) transactions: "%listJson)
        (take 5 txIds)
    reifyMsgLimit (Proxy @(InvOrData TxIdT TxMsgContents)) $
      \(_ :: Proxy s) -> withConnectionTo sendActions node $ \_ -> pure $ Conversation $
       \(conv :: (ConversationActions
                                  (ReqMsg TxIdT)
                                  (SmartLimit s (InvOrData TxIdT TxMsgContents))
                                  m)
        ) -> do
            let getTx id = do
                    logDebug $ sformat ("Requesting transaction "%build) id
                    send conv $ ReqMsg id
                    dt' <- recv conv
                    case withLimitedLength <$> dt' of
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

#endif
