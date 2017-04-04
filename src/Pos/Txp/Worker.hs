{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Txp.Worker
       ( txpWorkers
       ) where

import           Universum

import           Data.IORef        (newIORef, readIORef, writeIORef)
import           Data.Time.Units   (Second, convertUnit)
import           Formatting        (build, sformat, shown, (%))
import           Mockable          (delay, throw)
import           System.Wlog       (logWarning)

import           Pos.Communication (ConversationActions (..), DataMsg (..), InvMsg (..),
                                    InvOrData, Limit, LimitType, LimitedLengthExt,
                                    MempoolMsg (..), OutSpecs,
                                    RelayError (UnexpectedData, UnexpectedInv),
                                    RelayProxy (..), ReqMsg (..), SendActions,
                                    TxMsgContents, TxMsgTag (..), WorkerSpec, convH,
                                    handleDataL, handleInvL, reifyMsgLimit, toOutSpecs,
                                    withLimitedLength, worker)
import           Pos.Constants     (isDevelopment)
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
            ++ [ queryTxsWorker | not isDevelopment ]
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
queryTxsWorker = worker requestTxsOuts $ \sendActions -> do
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
requestTxsOuts :: OutSpecs
requestTxsOuts =
    toOutSpecs [ convH (Proxy @(MempoolMsg TxMsgTag))
                       (Proxy @(InvOrData TxMsgTag TxId TxMsgContents)) ]

-- | Send a MempoolMsg to a node and receive incoming 'InvMsg's with
-- transaction IDs.
getTxMempoolInvs
    :: WorkMode ssc m
    => SendActions m -> DHTNode -> m [TxId]
getTxMempoolInvs sendActions node =
    reifyMsgLimit (Proxy @(InvOrData TxMsgTag TxId TxMsgContents)) $
      \(_ :: Proxy s) -> converseToNode sendActions node $ \_
        (ConversationActions{..}::(ConversationActions
                                  (MempoolMsg TxMsgTag)
                                  (InvOrDataLimitedLength s TxId TxMsgTag TxMsgContents)
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
requestTxs sendActions node txIds =
    reifyMsgLimit (Proxy @(InvOrData TxMsgTag TxId TxMsgContents)) $
      \(_ :: Proxy s) -> converseToNode sendActions node $ \_
        (ConversationActions{..}::(ConversationActions
                                  (ReqMsg TxId TxMsgTag)
                                  (InvOrDataLimitedLength s TxId TxMsgTag TxMsgContents)
                                  m)
        ) -> do
            let txProxy = RelayProxy :: RelayProxy TxId TxMsgTag TxMsgContents
            let getTx id = do
                    send $ ReqMsg TxMsgTag id
                    dt' <- recv
                    case withLimitedLength <$> dt' of
                        Nothing -> error "didn't get an answer to Req"
                        Just x  -> expectRight (handleDataL txProxy) x
            for_ txIds $ \id ->
                getTx id `catch` handler id
  where
    expectRight _ (Left _)       = throw UnexpectedInv
    expectRight call (Right msg) = call msg

    handler id (e :: SomeException) = do
        logWarning $ sformat
            ("Couldn't get transaction with id "%build%" "%
             "from node "%build%": "%shown)
            id node e

-- | Type `InvOrData` with limited length. Was, too, copied from
-- Pos.Communication.Relay.Logic.
type InvOrDataLimitedLength s key tag contents =
    LimitedLengthExt s
        (Limit (InvMsg key tag), LimitType (DataMsg contents))
        (InvOrData tag key contents)

#endif
