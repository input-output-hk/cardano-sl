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
       , relayPropagateOut
       , InvOrData
       , handleDataDo
       , handleInvDo

       , invReqDataFlow
       , invReqDataFlowTK
       , invReqDataFlowNeighbors
       , invReqDataFlowNeighborsTK
       , addToRelayQueue
       , dataFlow
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
import           Pos.Communication.Limits.Types     (LimitedLength, MessageLimited,
                                                     recvLimited, reifyMsgLimit,
                                                     withLimitedLength')
import           Pos.Communication.Listener         (SizedCAHandler (..), convToSProxy,
                                                     listenerConv)
import           Pos.Communication.PeerState        (WithPeerState)
import           Pos.Communication.Protocol         (Conversation (..),
                                                     ConversationActions (..),
                                                     ListenerSpec, MkListeners, NodeId,
                                                     OutSpecs, SendActions (..),
                                                     WorkerSpec, constantListeners, convH,
                                                     toOutSpecs, worker)
import           Pos.Communication.Relay.Class      (DataParams (..),
                                                     InvReqDataParams (..),
                                                     MempoolParams (..), MonadRelayMem,
                                                     Relay (..), askRelayMem)
import           Pos.Communication.Relay.Types      (PropagationMsg (..),
                                                     RelayContext (..))
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
    -> (ListenerSpec m, OutSpecs)
handleReqL handleReq = listenerConv $ \__ourVerInfo ->
  SizedCAHandler $ \__nodeId conv ->
      let handlingLoop = do
              mbMsg <- fmap (withLimitedLength' $ convToSProxy conv) <$> recv conv
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
    -> [(ListenerSpec m, OutSpecs)]
handleMempoolL NoMempool = []
handleMempoolL (KeyMempool tagP handleMempool) = pure $ listenerConv $ \__ourVerInfo ->
  SizedCAHandler $ \__nodeId conv ->
      let handlingLoop = do
              mbMsg <- fmap (withLimitedLength' $ convToSProxy conv) <$> recv conv
              whenJust mbMsg $ \msg@MempoolMsg -> do
                  let _ = msg `asProxyTypeOf` mmP
                  res <- handleMempool
                  case nonEmpty res of
                      Nothing ->
                          logDebug $ sformat
                              ("We don't have mempool data "%shown) (typeRep tagP)
                      Just xs -> do
                          logDebug $ sformat ("We have mempool data "%shown) (typeRep tagP)
                          mapM_ (send conv . InvMsg) xs
                  handlingLoop
       in handlingLoop
  where
    mmP = (const Proxy :: Proxy tag -> Proxy (MempoolMsg tag)) tagP

handleDataOnlyL
    :: forall contents m .
       ( Bi (DataMsg contents)
       , Message Void
       , Message (DataMsg contents)
       , Buildable contents
       , RelayWorkMode m
       , MonadGStateCore m
       , MessageLimited (DataMsg contents)
       )
    => (contents -> m Bool)
    -> (ListenerSpec m, OutSpecs)
handleDataOnlyL handleData = listenerConv $ \__ourVerInfo ->
  SizedCAHandler (\__nodeId conv ->
      let handlingLoop = do
              mbMsg <- fmap (withLimitedLength' $ convToSProxy conv) <$> recv conv
              whenJust mbMsg $ \DataMsg{..} -> do
                  ifM (handleData dmContents)
                      (propagateData $ DataOnlyPM dmContents)
                      (logUseless dmContents)
                  handlingLoop
       in handlingLoop) :: SizedCAHandler Void (DataMsg contents) m
  where
    logUseless dmContents = logWarning $ sformat
        ("Ignoring data "%build) dmContents

-- Returns True if we should propagate.
handleDataDo
    :: forall key contents m .
       ( RelayWorkMode m
       , Buildable key
       , Eq key
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
        (propagateData $ InvReqDataPM dmKey dmContents) $
            logDebug $ sformat
                ("Ignoring data "%build%" for key "%build) dmContents dmKey

propagateData
    :: RelayWorkMode m
    => PropagationMsg
    -> m ()
propagateData pm = do
    shouldPropagate <- _rlyIsPropagation <$> askRelayMem
    if shouldPropagate then do
        addToRelayQueue pm
        logInfo $ sformat
            ("Adopted data, pushed "%build%" to propagation queue...")
            pm
    else logInfo $ sformat ("Adopted data "%build%", propagation is off") pm

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

relayListenersOne
  :: forall m.
     ( Mockable Throw m
     , WithLogger m
     , RelayWorkMode m
     , MonadGStateCore m
     , Message Void
     )
  => Relay m -> MkListeners m
relayListenersOne (InvReqData mP irdP@InvReqDataParams{..}) =
    constantListeners $
    [handleReqL handleReq, invDataListener irdP] ++ handleMempoolL mP
relayListenersOne (Data DataParams{..}) =
    constantListeners $
    [handleDataOnlyL handleDataOnly]

relayListeners
  :: forall m.
     ( Mockable Throw m
     , WithLogger m
     , RelayWorkMode m
     , MonadGStateCore m
     , Message Void
     )
  => [Relay m] -> MkListeners m
relayListeners = mconcat . map relayListenersOne

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
     , Eq key
     , MessageLimited (DataMsg contents)
     )
  => InvReqDataParams key contents m
  -> (ListenerSpec m, OutSpecs)
invDataListener InvReqDataParams{..} = listenerConv $ \__ourVerInfo ->
  SizedCAHandler $ \__nodeId conv ->
      let limit = withLimitedLength' $ convToSProxy conv
          handlingLoop = do
              inv' <- fmap limit <$> recv conv
              whenJust inv' $ expectInv $ \InvMsg{..} -> do
                  useful <- handleInvDo handleInv imKey
                  whenJust useful $ \ne -> do
                      send conv $ ReqMsg ne
                      dt' <- fmap limit <$> recv conv
                      whenJust dt' $ expectData $ \DataMsg{..} -> do
                            handleDataDo contentsToKey handleData dmContents
                            -- handlingLoop

                            -- TODO CSL-1148 Improve relaing: support multiple data
                            -- Need to receive Inv and Data messages simultaneously
                            -- Maintain state of sent Reqs
                            -- And check data we are sent is what we expect (currently not)
       in handlingLoop

addToRelayQueue
    :: forall m.
       RelayWorkMode m
    => PropagationMsg -> m ()
addToRelayQueue pm = do
    queue <- _rlyPropagationQueue <$> askRelayMem
    isFull <- atomically $ isFullTBQueue queue
    if isFull then
        logWarning $ "Propagation queue is full, no propagation"
    else
        atomically $ writeTBQueue queue pm

relayPropagateOut :: Message Void => [Relay m] -> OutSpecs
relayPropagateOut = mconcat . map propagateOutImpl

propagateOutImpl :: Message Void => Relay m -> OutSpecs
propagateOutImpl (InvReqData _ irdp) = toOutSpecs
      [ convH invProxy reqProxy
      ]
  where
    invProxy = (const Proxy :: InvReqDataParams key contents m
                            -> Proxy (InvOrData key contents)) irdp
    reqProxy = (const Proxy :: InvReqDataParams key contents m
                            -> Proxy (ReqMsg key)) irdp
propagateOutImpl (Data dp) = toOutSpecs
      [ convH dataProxy (Proxy @Void)
      ]
  where
    dataProxy = (const Proxy :: DataParams contents m
                            -> Proxy (DataMsg contents)) dp

relayWorkers
    :: forall m.
       ( Mockable Throw m
       , MonadDiscovery m
       , RelayWorkMode m
       , MonadMask m
       , MonadReportingMem m
       , Message Void
       )
    => [Relay m] -> ([WorkerSpec m], OutSpecs)
relayWorkers rls = relayWorkersImpl $ relayPropagateOut rls

relayWorkersImpl
    :: forall m.
       ( Mockable Throw m
       , MonadDiscovery m
       , RelayWorkMode m
       , MonadMask m
       , MonadReportingMem m
       , Message Void
       )
    => OutSpecs -> ([WorkerSpec m], OutSpecs)
relayWorkersImpl allOutSpecs =
    first (:[]) $ worker allOutSpecs $ \sendActions ->
        handleAll handleWE $ reportingFatal version $ action sendActions
  where
    action sendActions = do
        queue <- _rlyPropagationQueue <$> askRelayMem
        forever $ atomically (readTBQueue queue) >>= \case
            InvReqDataPM key contents -> do
                logDebug $ sformat
                    ("Propagation data with key: "%build) key
                converseToNeighbors sendActions $ \__node ->
                    pure $ Conversation $ irdHandler key contents
            DataOnlyPM contents -> do
                logDebug $ sformat
                    ("Propagation data: "%build) contents
                converseToNeighbors sendActions $ \__node ->
                    pure $ Conversation $ doHandler contents

    doHandler
        :: contents1
        -> ConversationActions
             (DataMsg contents1) Void m
        -> m ()
    doHandler contents conv = send conv $ DataMsg contents

    irdHandler
        :: Eq key1 => key1 -> contents1
        -> ConversationActions
             (InvOrData key1 contents1) (ReqMsg key1) m
        -> m ()
    irdHandler key conts conv = do
        send conv $ Left $ InvMsg key
        let whileNotK = do
              rm <- recv conv
              whenJust rm $ \ReqMsg{..} -> do
                if rmKey == key
                   then send conv $ Right $ DataMsg conts
                   else whileNotK
        whileNotK

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
        withConnectionTo sendActions addr $
        const $ pure $ Conversation $ invReqDataFlowDo what key dt lim addr
  where
    handleE e = logWarning $
        sformat ("Error sending "%stext%", key = "%build%" to "%shown%": "%shown)
                what key addr e

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
    handleD =
        logDebug $
        sformat ("InvReqDataFlow ("%stext%"): "%shown %" closed conversation on \
                 \Inv key = "%build)
                what nodeId key

dataFlow
    :: forall contents m.
       ( Message (DataMsg contents)
       , Bi (DataMsg contents)
       , Buildable contents
       , MinRelayWorkMode m
       , Message Void
       )
    => Text -> SendActions m -> NodeId -> contents -> m ()
dataFlow what sendActions addr dt = handleAll handleE $
    withConnectionTo sendActions addr
      (const (pure $ Conversation $ \(conv :: ConversationActions (DataMsg contents) Void m) ->
                          send conv $ DataMsg dt))
  where
    handleE e =
        logWarning $
        sformat ("Error sending "%stext%", data = "%build%" to "%shown%": "%shown)
                what dt addr e
