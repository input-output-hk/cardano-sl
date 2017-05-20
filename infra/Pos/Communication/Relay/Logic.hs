-- | Framework for Inv\/Req\/Data message handling

{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Pos.Communication.Relay.Logic
       ( Relay (..)
       , InvMsg (..)
       , ReqMsg (..)
       , MempoolMsg (..)
       , DataMsg (..)
       , relayListeners
       , relayWorkers
       , InvOrData
       , handleDataDo
       , handleInvDo

       , invReqDataFlow
       , invReqDataFlowTK
       , invReqDataFlowNeighbors
       , invReqDataFlowNeighborsTK
       ) where

import           Control.Concurrent.STM             (isFullTBQueue, readTBQueue,
                                                     writeTBQueue)
import           Data.Proxy                         (asProxyTypeOf)
import           Data.Tagged                        (Tagged, tagWith)
import           Data.Typeable                      (typeRep)
import           Formatting                         (build, sformat, shown, stext, (%))
import           Mockable                           (Mockable, MonadMockable, Throw,
                                                     handleAll, throw, throw)
import           Node.Message                       (Message)
import           Paths_cardano_sl_infra             (version)
import           System.Wlog                        (WithLogger, logDebug, logError,
                                                     logInfo, logWarning)
import           Universum

import           Pos.Binary.Class                   (Bi (..))
import           Pos.Binary.Infra.Communication     ()
import           Pos.Communication.Limits.Instances ()
import           Pos.Communication.Limits.Types     (LimitedLength, LimitedLengthExt (..),
                                                     MessageLimited, SmartLimit,
                                                     recvLimited, reifyMsgLimit,
                                                     withLimitedLength)
import           Pos.Communication.PeerState        (WithPeerState)
import           Pos.Communication.Protocol         (Conversation (..),
                                                     ConversationActions (..),
                                                     ListenerSpec, NodeId, OutSpecs,
                                                     SendActions (..), WorkerSpec,
                                                     listenerConv, mergeLs, worker)
import           Pos.Communication.Relay.Class      (InvReqDataParams (..),
                                                     MempoolParams (..), MonadRelayMem,
                                                     Relay (..), askRelayMem)
import           Pos.Communication.Relay.Types      (RelayContext (..), SomeInvMsg (..))
import           Pos.Communication.Relay.Util       (expectData, expectInv)
import           Pos.Communication.Types.Relay      (DataMsg (..), InvMsg (..), InvOrData,
                                                     MempoolMsg (..), ReqMsg (..))
import           Pos.DB.Class                       (MonadGStateCore)
import           Pos.Discovery.Broadcast            (converseToNeighbors)
import           Pos.Discovery.Class                (MonadDiscovery)
import           Pos.Reporting                      (MonadReportingMem, reportingFatal)

type MinRelayWorkMode m =
    ( WithLogger m
    , MonadMockable m
    , MonadIO m
    , WithPeerState m
    )

type RelayWorkMode m =
    ( MinRelayWorkMode m
    , MonadRelayMem m
    )

handleReqL
    :: forall key contents m .
       ( Bi (ReqMsg key)
       , Bi (InvOrData key contents)
       , Message (InvOrData key contents)
       , Message (ReqMsg key)
       , Buildable key
       , MinRelayWorkMode m
       , MonadGStateCore m
       )
    => (key -> m (Maybe contents))
    -> m (ListenerSpec m, OutSpecs)
handleReqL handleReq = reifyMsgLimit (Proxy @(ReqMsg key)) $
  \(_ :: Proxy s) -> return $ listenerConv $
    \_ __peerId conv ->
    let handlingLoop = do
            mbMsg <- fmap (withLimitedLength @s) <$> recv conv
            whenJust mbMsg $ \ReqMsg{..} -> do
                dtMB <- handleReq rmKey
                case dtMB of
                    Nothing -> logNoData rmKey
                    Just dt -> logHaveData rmKey >> send conv (constructDataMsg dt)
                handlingLoop
    in handlingLoop
  where
    constructDataMsg :: contents -> InvOrData key contents
    constructDataMsg = Right . DataMsg
    logNoData rmKey = logDebug $ sformat
        ("We don't have data for key "%build)
        rmKey
    logHaveData rmKey= logDebug $ sformat
        ("We have data for key "%build)
        rmKey

handleMempoolL
    :: forall m.
       ( MinRelayWorkMode m
       , MonadGStateCore m
       )
    => MempoolParams m
    -> [m (ListenerSpec m, OutSpecs)]
handleMempoolL NoMempool = []
handleMempoolL (KeyMempool tagP handleMempool) = pure $ reifyMsgLimit mmP $
  \(_ :: Proxy s) -> return $ listenerConv $
    \_ __peerId conv ->
    whenJustM (recv conv) $ \(withLimitedLength @s -> msg@MempoolMsg) -> do
      let _ = msg `asProxyTypeOf` mmP
      res <- handleMempool
      case nonEmpty res of
          Nothing ->
              logDebug $ sformat
                  ("We don't have mempool data "%shown) (typeRep tagP)
          Just xs -> do
              logDebug $ sformat ("We have mempool data "%shown) (typeRep tagP)
              mapM_ (send conv . InvMsg) xs
  where
    mmP = (const Proxy :: Proxy tag -> Proxy (MempoolMsg tag)) tagP

-- Returns True if we should propagate.
handleDataDo
    :: forall key contents m .
       ( RelayWorkMode m
       , Buildable key
       , Buildable contents
       , Message (InvOrData key contents)
       , Message (ReqMsg key)
       , Bi (InvOrData key contents)
       , Bi (ReqMsg key)
       )
    => (contents -> m key)
    -> (contents -> m Bool)
    -> contents
    -> m ()
handleDataDo contentsToKey handleData dmContents = do
    dmKey <- contentsToKey dmContents
    ifM (handleData dmContents)
        (propagate dmKey) $
            logDebug $ sformat
                ("Ignoring data "%build%" for key "%build) dmContents dmKey
  where
    propagate dmKey = do
        shouldPropagate <- _rlyIsPropagation <$> askRelayMem
        if shouldPropagate then do
            let inv :: InvOrData key contents
                inv = Left $ InvMsg dmKey
            addToRelayQueue inv
            logInfo $ sformat
                ("Adopted data "%build%" "%
                  "for key "%build%", data has been pushed to propagation queue...")
                dmContents dmKey
        else
            logInfo $ sformat
                ("Adopted data "%build%" for "%
                  "key "%build%", no propagation")
                dmContents dmKey

handleInvDo
    :: forall key m .
       ( RelayWorkMode m
       , Buildable key
       )
    => (key -> m Bool)
    -> key
    -> m (Maybe key)
handleInvDo handleInv imKey =
    ifM (handleInv imKey)
        (Just imKey <$ logUseful)
        (Nothing <$ logUseless)
  where
    logUseless = logDebug $ sformat
        ("Ignoring inv for key "%build%", because it's useless")
        imKey
    logUseful = logDebug $ sformat
        ("We'll request data for key "%build%", because it's useful")
        imKey

relayListeners
  :: forall m.
     ( Mockable Throw m
     , WithLogger m
     , RelayWorkMode m
     , MonadGStateCore m
     )
  => Relay m -> m ([ListenerSpec m], OutSpecs)
relayListeners (InvReqData mP irdP@InvReqDataParams{..}) =
    (fmap mergeLs . sequence) $
        [handleReqL handleReq, invDataListener irdP] ++ handleMempoolL mP

invDataListener
  :: forall m key contents.
     ( RelayWorkMode m
     , MonadGStateCore m
     , Message (ReqMsg key)
     , Message (InvOrData key contents)
     , Bi (ReqMsg key)
     , Bi (InvOrData key contents)
     , Buildable contents
     , Buildable key
     , MessageLimited (DataMsg contents)
     )
  => InvReqDataParams key contents m
  -> m (ListenerSpec m, OutSpecs)
invDataListener InvReqDataParams{..} = reifyMsgLimit (Proxy @(InvOrData key contents)) $
  \(_ :: Proxy s) -> return $ listenerConv $ \_ __peerId
    (conv ::(ConversationActions
                              (ReqMsg key)
                              (SmartLimit s (InvOrData key contents))
                              m)
    ) -> do
        inv' <- recv conv
        whenJust (withLimitedLength <$> inv') $ expectInv $
            \InvMsg{..} -> do
                useful <- handleInvDo handleInv imKey
                whenJust useful $ \ne -> do
                    send conv $ ReqMsg ne
                    dt' <- recv conv
                    whenJust (withLimitedLength <$> dt') $ expectData $
                        \DataMsg{..} -> handleDataDo contentsToKey handleData dmContents

addToRelayQueue
    :: forall key contents m.
       ( Bi (InvOrData key contents)
       , Bi (ReqMsg key)
       , Message (InvOrData key contents)
       , Message (ReqMsg key)
       , Buildable key
       , RelayWorkMode m
       )
    => InvOrData key contents -> m ()
addToRelayQueue inv = do
    queue <- _rlyPropagationQueue <$> askRelayMem
    isFull <- atomically $ isFullTBQueue queue
    if isFull then
        logWarning $ "Propagation queue is full, no propagation"
    else
        atomically $ writeTBQueue queue (SomeInvMsg inv)

relayWorkers
    :: forall m.
       ( Mockable Throw m
       , MonadDiscovery m
       , RelayWorkMode m
       , MonadMask m
       , MonadReportingMem m
       )
    => OutSpecs -> ([WorkerSpec m], OutSpecs)
relayWorkers allOutSpecs =
    first (:[]) $ worker allOutSpecs $ \sendActions ->
        handleAll handleWE $ reportingFatal version $ action sendActions
  where
    action sendActions = do
        queue <- _rlyPropagationQueue <$> askRelayMem
        forever $ atomically (readTBQueue queue) >>= \case
            SomeInvMsg i@(Left (InvMsg{..})) -> do
                logDebug $ sformat
                    ("Propagation data with key: "%build) imKey
                converseToNeighbors sendActions $ \__node -> pure $ Conversation $ convHandler i
            SomeInvMsg (Right _) ->
                logWarning $ "DataMsg is contains in inv propagation queue"

    convHandler
        :: InvOrData key1 contents1
        -> ConversationActions
             (InvOrData key1 contents1) (ReqMsg key1) m
        -> m ()
    convHandler inv conv = send conv inv

    handleWE e = do
        logError $ sformat ("relayWorker: error caught "%shown) e
        throw e

----------------------------------------------------------------------------
-- Helpers for Communication.Methods
----------------------------------------------------------------------------

invReqDataFlowNeighborsTK
    :: forall key contents m.
       ( Message (InvOrData (Tagged contents key) contents)
       , Message (ReqMsg (Tagged contents key))
       , Buildable key
       , Typeable contents
       , MinRelayWorkMode m
       , MonadGStateCore m
       , MonadDiscovery m
       , Bi (InvOrData (Tagged contents key) contents)
       , Bi (ReqMsg (Tagged contents key))
       )
    => Text -> SendActions m -> key -> contents -> m ()
invReqDataFlowNeighborsTK what sendActions key dt =
    invReqDataFlowNeighbors what sendActions key' dt
  where
    contProxy = (const Proxy :: contents -> Proxy contents) dt
    key' = tagWith contProxy key

invReqDataFlowTK
    :: forall key contents m.
       ( Message (InvOrData (Tagged contents key) contents)
       , Message (ReqMsg (Tagged contents key))
       , Buildable key
       , Typeable contents
       , MinRelayWorkMode m
       , MonadGStateCore m
       , Bi (InvOrData (Tagged contents key) contents)
       , Bi (ReqMsg (Tagged contents key))
       )
    => Text -> SendActions m -> NodeId -> key -> contents -> m ()
invReqDataFlowTK what sendActions addr key dt =
    invReqDataFlow what sendActions addr key' dt
  where
    contProxy = (const Proxy :: contents -> Proxy contents) dt
    key' = tagWith contProxy key

invReqDataFlowNeighbors
    :: forall key contents m.
       ( Message (InvOrData key contents)
       , Message (ReqMsg key)
       , Buildable key
       , MinRelayWorkMode m
       , MonadGStateCore m
       , MonadDiscovery m
       , Bi (InvOrData key contents)
       , Bi (ReqMsg key)
       )
    => Text -> SendActions m -> key -> contents -> m ()
invReqDataFlowNeighbors what sendActions key dt = handleAll handleE $
    reifyMsgLimit (Proxy @(ReqMsg key)) $ \lim -> do
        converseToNeighbors sendActions (pure . Conversation . invReqDataFlowDo what key dt lim)
  where
    handleE e = logWarning $
        sformat ("Error sending "%stext%", key = "%build%" to neighbors: "%shown) what key e

invReqDataFlow
    :: forall key contents m.
       ( Message (InvOrData key contents)
       , Message (ReqMsg key)
       , Bi (InvOrData key contents)
       , Bi (ReqMsg key)
       , Buildable key
       , MinRelayWorkMode m
       , MonadGStateCore m
       )
    => Text -> SendActions m -> NodeId -> key -> contents -> m ()
invReqDataFlow what sendActions addr key dt = handleAll handleE $
    reifyMsgLimit (Proxy @(ReqMsg key)) $ \lim ->
        withConnectionTo sendActions addr (const (pure $ Conversation $ invReqDataFlowDo what key dt lim addr))
  where
    handleE e = logWarning $
        sformat ("Error sending "%stext%", key = "%build%" to "%shown%": "%shown) what key addr e

invReqDataFlowDo
    :: ( Message (InvOrData key contents)
       , Message (ReqMsg key)
       , Bi (InvOrData key contents)
       , Bi (ReqMsg key)
       , Buildable key
       , MinRelayWorkMode m
       , MonadGStateCore m
       )
    => Text
    -> key
    -> contents
    -> Proxy s
    -> NodeId
    -> ConversationActions (InvOrData key contents) (LimitedLength s (ReqMsg key)) m
    -> m ()
invReqDataFlowDo what key dt _ nodeId conv = do
    send conv $ Left $ InvMsg key
    recvLimited conv >>= maybe handleD replyWithData
  where
    -- TODO need to check we're asked for same key we have
    replyWithData (ReqMsg _) = send conv $ Right $ DataMsg dt
    handleD = logDebug $
        sformat ("InvReqDataFlow ("%stext%"): "%shown %" closed conversation on \
                 \Inv key = "%build) what nodeId key
