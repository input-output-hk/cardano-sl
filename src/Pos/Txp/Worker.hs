{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Txp.Worker
       ( txpWorkers
       ) where

import           Universum

import           Data.IORef        (newIORef, readIORef, writeIORef)
import           Data.Time.Units   (Second, convertUnit)
import           Formatting        (build, int, sformat, shown, (%))
import           Mockable          (delay, throw)
import           Serokell.Util     (listJson)
import           System.Wlog       (logDebug, logInfo, logWarning)

import           Pos.Communication (ConversationActions (..), InvMsg (..), InvOrData,
                                    MempoolMsg (..), OutSpecs,
                                    RelayError (UnexpectedData, UnexpectedInv),
                                    RelayProxy (..), ReqMsg (..), SendActions, SmartLimit,
                                    TxMsgContents, TxMsgTag (..), WorkerSpec, convH,
                                    handleDataL, handleInvL, reifyMsgLimit, toOutSpecs,
                                    withLimitedLength, worker)
import           Pos.DHT           (DHTNode, converseToNode, getKnownPeers)
import           Pos.Slotting      (getLastKnownSlotDuration)
import           Pos.Ssc.Class     (SscWorkersClass)
import           Pos.Txp.Core      (TxId)
import           Pos.Util          (mconcatPair)
import           Pos.WorkMode      (WorkMode)

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
    nodesRef <- liftIO . newIORef =<< getKnownPeers
    let delayInterval = max (slotDur `div` 4) (convertUnit (5 :: Second))
        action = forever $ do
            -- If we ran out of nodes to query, refresh the list
            whenM (null <$> liftIO (readIORef nodesRef)) $
                liftIO . writeIORef nodesRef =<< getKnownPeers
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
          convH (Proxy @(MempoolMsg TxMsgTag))
                (Proxy @(InvOrData TxMsgTag TxId TxMsgContents))
          -- used by 'requestTxs'
        , convH (Proxy @(ReqMsg TxId TxMsgTag))
                (Proxy @(InvOrData TxMsgTag TxId TxMsgContents))
        ]

-- | Send a MempoolMsg to a node and receive incoming 'InvMsg's with
-- transaction IDs.
getTxMempoolInvs
    :: WorkMode ssc m
    => SendActions m -> DHTNode -> m [TxId]
getTxMempoolInvs sendActions node = do
    logInfo ("Querying tx mempool from node " <> show node)
    reifyMsgLimit (Proxy @(InvOrData TxMsgTag TxId TxMsgContents)) $
      \(_ :: Proxy s) -> converseToNode sendActions node $ \_
        (ConversationActions{..}::(ConversationActions
                                  (MempoolMsg TxMsgTag)
                                  (SmartLimit s (InvOrData TxMsgTag TxId TxMsgContents))
                                  m)
        ) -> do
            let txProxy = RelayProxy :: RelayProxy TxId TxMsgTag TxMsgContents
            send $ MempoolMsg TxMsgTag
            let getInvs = do
                  inv' <- recv
                  case withLimitedLength <$> inv' of
                      Nothing -> return []
                      Just (Right _) -> throw UnexpectedData
                      Just (Left inv@InvMsg{..}) -> do
                          useful <- handleInvL txProxy inv
                          case useful of
                              Nothing  -> getInvs
                              Just key -> (key:) <$> getInvs
            getInvs

-- | Request several transactions.
requestTxs
    :: WorkMode ssc m
    => SendActions m -> DHTNode -> [TxId] -> m ()
requestTxs sendActions node txIds = do
    logInfo $ sformat
        ("Requesting "%int%" txs from node "%shown)
        (length txIds) node
    logDebug $ sformat
        ("First 5 (or less) transactions: "%listJson)
        (take 5 txIds)
    reifyMsgLimit (Proxy @(InvOrData TxMsgTag TxId TxMsgContents)) $
      \(_ :: Proxy s) -> converseToNode sendActions node $ \_
        (ConversationActions{..}::(ConversationActions
                                  (ReqMsg TxId TxMsgTag)
                                  (SmartLimit s (InvOrData TxMsgTag TxId TxMsgContents))
                                  m)
        ) -> do
            let txProxy = RelayProxy :: RelayProxy TxId TxMsgTag TxMsgContents
            let getTx id = do
                    logDebug $ sformat ("Requesting transaction "%build) id
                    send $ ReqMsg TxMsgTag id
                    dt' <- recv
                    case withLimitedLength <$> dt' of
                        Nothing -> error "didn't get an answer to Req"
                        Just x  -> expectRight (handleDataL txProxy) x
            for_ txIds $ \id ->
                getTx id `catch` handler id
    logInfo $ sformat
        ("Finished requesting txs from node "%shown)
        node
  where
    expectRight _ (Left _)       = throw UnexpectedInv
    expectRight call (Right msg) = call msg

    handler id (e :: SomeException) = do
        logWarning $ sformat
            ("Couldn't get transaction with id "%build%" "%
             "from node "%build%": "%shown)
            id node e

#endif
