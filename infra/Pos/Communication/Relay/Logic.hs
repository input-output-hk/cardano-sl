-- | Framework for Inv\/Req\/Data message handling

{-# LANGUAGE ConstraintKinds     #-}
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
       , relayStubListeners
       , relayWorkers
       , RelayProxy (..)
       , InvOrData

       , handleInvL
       , handleReqL
       , handleMempoolL
       , handleDataL

       , invReqDataFlow
       , invReqDataFlowNeighbors
       ) where

import           Control.Concurrent.STM             (isFullTBQueue, readTBQueue,
                                                     writeTBQueue)
import           Formatting                         (build, sformat, shown, stext, (%))
import           Mockable                           (Mockable, MonadMockable, Throw,
                                                     handleAll, throw, throw)
import           Node.Message                       (Message)
import           Paths_cardano_sl_infra             (version)
import           Serokell.Util.Text                 (listJson)
import           Serokell.Util.Verify               (VerificationRes (..))
import           System.Wlog                        (WithLogger, logDebug, logError,
                                                     logInfo, logWarning)
import           Universum

import           Pos.Binary.Class                   (Bi (..))
import           Pos.Binary.Infra.Communication     ()
import           Pos.Communication.Limits.Types     (LimitedLength, LimitedLengthExt (..),
                                                     SmartLimit, recvLimited,
                                                     reifyMsgLimit, withLimitedLength)
import           Pos.Communication.MessagePart      (MessagePart)
import           Pos.Communication.PeerState        (WithPeerState)
import           Pos.Communication.Protocol         (ConversationActions (..),
                                                     ListenerSpec, NodeId, OutSpecs,
                                                     SendActions (..), WorkerSpec,
                                                     listenerConv, mergeLs, worker)
import           Pos.Communication.Relay.Class      (MonadRelayMem (..), Relay (..))
import           Pos.Communication.Relay.Types      (RelayContext (..), RelayProxy (..),
                                                     SomeInvMsg (..))
import           Pos.Communication.Relay.Util       (expectData, expectInv)
import           Pos.Communication.Types.Relay      (DataMsg (..), InvMsg (..), InvOrData,
                                                     MempoolMsg (..), ReqMsg (..))
import           Pos.Communication.Util             (stubListenerConv)
import           Pos.DB.Limits                      (MonadDBLimits)
import           Pos.Discovery.Neighbors            (converseToNeighbors)
import           Pos.Reporting                      (MonadReportingMem, reportingFatal)

import           Pos.Communication.Limits.Instances ()

type MinRelayWorkMode m
    = ( WithLogger m
      , MonadMockable m
      , MonadIO m
      , WithPeerState m
      )

type RelayWorkMode m = ( MinRelayWorkMode m, MonadRelayMem m)

-- Returns useful keys.
handleInvL
    :: forall m key tag contents.
       ( Bi (InvMsg key tag)
       , Bi (ReqMsg key tag)
       , Relay m tag key contents
       , MinRelayWorkMode m
       )
    => RelayProxy key tag contents
    -> InvMsg key tag
    -> m (Maybe key)
handleInvL proxy msg@(InvMsg{..}) =
    processMessage Nothing "Inventory" imTag verifyInvTag $ do
        let _ = invCatchType proxy msg
        invRes <- handleInv imTag imKey
        if invRes then
            Just imKey <$ logDebug (sformat
              ("We'll request data "%build%" for key "%build%", because it's useful")
              imTag imKey)
        else
            Nothing <$ logDebug (sformat
              ("Ignoring inv "%build%" for key "%build%", because it's useless")
              imTag imKey)

handleReqL
    :: forall key tag contents m .
       ( Bi (ReqMsg key tag)
       , Bi (InvOrData tag key contents)
       , Message (InvOrData tag key contents)
       , Relay m tag key contents
       , MinRelayWorkMode m
       , MonadDBLimits m
       )
    => RelayProxy key tag contents
    -> m (ListenerSpec m, OutSpecs)
handleReqL proxy = reifyMsgLimit (Proxy @(ReqMsg key tag)) $
  \(_ :: Proxy s) -> return $ listenerConv $
    \_ __peerId ConversationActions{..} ->
    let handlingLoop = do
            mbMsg <- fmap (withLimitedLength @s) <$> recv
            whenJust mbMsg $ \msg@ReqMsg{..} -> do
                let _ = reqCatchType proxy msg
                processMessage () "Request" rmTag verifyReqTag $ do
                    let logNoData = logDebug $ sformat
                            ("We don't have data "%build%" for key "%build)
                            rmTag rmKey
                        logHaveData = logDebug $ sformat
                            ("We have data "%build%" for key "%build)
                            rmTag rmKey
                    dtMB <- handleReq rmTag rmKey
                    case dtMB of
                        Nothing -> logNoData
                        Just dt -> logHaveData >> send (constructDataMsg dt)
                handlingLoop
    in handlingLoop
  where
    constructDataMsg :: contents -> InvOrData tag key contents
    constructDataMsg = Right . DataMsg

handleMempoolL
    :: forall key tag contents m .
       ( Bi (MempoolMsg tag)
       , Bi (InvOrData tag key contents)
       , Message (InvOrData tag key contents)
       , Relay m tag key contents
       , MinRelayWorkMode m
       , MonadDBLimits m
       )
    => RelayProxy key tag contents
    -> m (ListenerSpec m, OutSpecs)
handleMempoolL proxy = reifyMsgLimit (Proxy @(MempoolMsg tag)) $
  \(_ :: Proxy s) -> return $ listenerConv $
    \_ __peerId ConversationActions{..} ->
    whenJustM recv $ \(withLimitedLength @s -> msg@MempoolMsg{..}) -> do
      let _ = mempoolCatchType proxy msg
      processMessage () "Mempool" mmTag verifyMempoolTag $ do
          res <- handleMempool mmTag
          case nonEmpty res of
              Nothing -> logDebug $ sformat
                  ("We don't have mempool data "%build) mmTag
              Just xs -> do
                  logDebug $ sformat ("We have mempool data "%build) mmTag
                  mapM_ (send . constructInvMsg mmTag) xs
  where
    constructInvMsg :: tag -> key -> InvOrData tag key contents
    constructInvMsg tag = Left . InvMsg tag

-- Returns True if we should propagate.
handleDataL
      :: forall tag key contents m .
      ( Bi (InvMsg key tag)
      , Bi (ReqMsg key tag)
      , Bi key
      , Bi tag
      , Bi (DataMsg contents)
      , MessagePart tag
      , MessagePart contents
      , Relay m tag key contents
      , RelayWorkMode m
      )
    => RelayProxy key tag contents
    -> DataMsg contents
    -> m ()
handleDataL proxy msg@(DataMsg {..}) =
    processMessage () "Data" dmContents verifyDataContents $ do
        let _ = dataCatchType proxy msg
        dmKey <- contentsToKey dmContents
        ifM (handleData dmContents)
            (handleDataLDo dmKey) $
                logDebug $ sformat
                    ("Ignoring data "%build%" for key "%build) dmContents dmKey
  where
    handleDataLDo dmKey = do
        shouldPropagate <- _rlyIsPropagation <$> askRelayMem
        if shouldPropagate then do
            tag <- contentsToTag dmContents
            let inv :: InvOrData tag key contents
                inv = Left $ InvMsg tag dmKey
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

processMessage
  :: (Buildable param, WithLogger m)
  => a -> Text -> param -> (param -> m VerificationRes) -> m a -> m a
processMessage defaultRes name param verifier action = do
    verRes <- verifier param
    case verRes of
      VerSuccess -> action
      VerFailure reasons ->
          defaultRes <$
              logWarning (sformat
                ("Wrong "%stext%": invalid "%build%": "%listJson)
                name param reasons)

relayListeners
  :: forall m key tag contents.
     ( Bi key
     , Bi tag
     , Bi (InvMsg key tag)
     , Bi (DataMsg contents)
     , Bi (ReqMsg key tag)
     , Bi (MempoolMsg tag)
     , Bi (InvOrData tag key contents)
     , MessagePart contents
     , MessagePart tag
     , Relay m tag key contents
     , Mockable Throw m
     , WithLogger m
     , RelayWorkMode m
     , MonadDBLimits m
     )
  => RelayProxy key tag contents -> m ([ListenerSpec m], OutSpecs)
relayListeners proxy =
    mergeLs <$> sequence
        [handleReqL proxy, handleMempoolL proxy, invDataListener]
  where
    invDataListener = reifyMsgLimit (Proxy @(InvOrData tag key contents)) $
      \(_ :: Proxy s) -> return $ listenerConv $ \_ __peerId
        (ConversationActions{..}::(ConversationActions
                                  (ReqMsg key tag)
                                  (SmartLimit s (InvOrData tag key contents))
                                  m)
        ) -> do
            inv' <- recv
            whenJust (withLimitedLength <$> inv') $ expectInv $
                \inv@InvMsg{..} -> do
                    useful <- handleInvL proxy inv
                    whenJust useful $ \ne -> do
                        send $ ReqMsg imTag ne
                        dt' <- recv
                        whenJust (withLimitedLength <$> dt') $ expectData $
                            \dt -> handleDataL proxy dt

relayStubListeners
    :: ( WithLogger m
       , Bi (InvMsg key tag)
       , Bi (ReqMsg key tag)
       , Bi (MempoolMsg tag)
       , Bi (DataMsg contents)
       , Message (InvOrData tag key contents)
       , Message (ReqMsg key tag)
       , Message (MempoolMsg tag)
       )
    => RelayProxy key tag contents -> ([ListenerSpec m], OutSpecs)
relayStubListeners p = mergeLs
    [ stubListenerConv $ invDataMsgProxy p
    , stubListenerConv $ reqMsgProxy p
    , stubListenerConv $ mempoolMsgProxy p
    ]

-- For stub listeners (e.g. reqMsgProxy), the first parameter in the Proxy is
-- “what the listener listens to” and the second is “what the listener sends
-- back”.

invCatchType :: RelayProxy key tag contents -> InvMsg key tag -> ()
invCatchType _ _ = ()

reqCatchType :: RelayProxy key tag contents -> ReqMsg key tag -> ()
reqCatchType _ _ = ()
reqMsgProxy :: RelayProxy key tag contents
            -> Proxy (ReqMsg key tag, InvOrData tag key contents)
reqMsgProxy _ = Proxy

mempoolCatchType :: RelayProxy key tag contents -> MempoolMsg tag -> ()
mempoolCatchType _ _ = ()
mempoolMsgProxy :: RelayProxy key tag contents
                -> Proxy (MempoolMsg tag, InvOrData tag key contents)
mempoolMsgProxy _ = Proxy

dataCatchType :: RelayProxy key tag contents -> DataMsg contents -> ()
dataCatchType _ _ = ()

invDataMsgProxy :: RelayProxy key tag contents
                -> Proxy (InvOrData tag key contents, ReqMsg key tag)
invDataMsgProxy _ = Proxy


addToRelayQueue :: forall tag key contents m .
                ( Bi (InvOrData tag key contents)
                , Bi (ReqMsg key tag)
                , Message (InvOrData tag key contents)
                , Message (ReqMsg key tag)
                , Buildable tag, Buildable key
                , RelayWorkMode m
                )
                => InvOrData tag key contents -> m ()
addToRelayQueue inv = do
    queue <- _rlyPropagationQueue <$> askRelayMem
    isFull <- atomically $ isFullTBQueue queue
    if isFull then
        logWarning $ "Propagation queue is full, no propagation"
    else
        atomically $ writeTBQueue queue (SomeInvMsg inv)

relayWorkers :: forall m .
             ( Mockable Throw m
             , RelayWorkMode m
             , MonadMask m
             , MonadReportingMem m
             )
             => m (Set NodeId) -> OutSpecs -> ([WorkerSpec m], OutSpecs)
relayWorkers getPeers allOutSpecs =
    first (:[]) $ worker allOutSpecs $ \sendActions ->
        handleAll handleWE $ reportingFatal getPeers version $ action sendActions
  where
    action sendActions = do
        queue <- _rlyPropagationQueue <$> askRelayMem
        forever $ atomically (readTBQueue queue) >>= \case
            SomeInvMsg i@(Left (InvMsg{..})) -> do
                logDebug $ sformat
                    ("Propagation data with key: "%build%
                     " and tag: "%build) imKey imTag
                peers <- getPeers
                converseToNeighbors peers sendActions (convHandler i)
            SomeInvMsg (Right _) ->
                logWarning $ "DataMsg is contains in inv propagation queue"

    convHandler
        :: InvOrData tag1 key1 contents1
        -> NodeId
        -> ConversationActions
             (InvOrData tag1 key1 contents1) (ReqMsg key1 tag1) m
        -> m ()
    convHandler inv __peerId ConversationActions{..} = send inv

    handleWE e = do
        logError $ sformat ("relayWorker: error caught "%shown) e
        throw e

----------------------------------------------------------------------------
-- Helpers for Communication.Methods
----------------------------------------------------------------------------

invReqDataFlowNeighbors
    :: forall tag id contents m.
    ( Message (InvOrData tag id contents)
    , Message (ReqMsg id tag)
    , Buildable id
    , MinRelayWorkMode m
    , MonadDBLimits m
    , Bi tag, Bi id
    , Bi (InvOrData tag id contents)
    , Bi (ReqMsg id tag))
    => m (Set NodeId) -> Text -> SendActions m -> tag -> id -> contents -> m ()
invReqDataFlowNeighbors getPeers what sendActions tag id dt = handleAll handleE $
    reifyMsgLimit (Proxy @(ReqMsg id tag)) $ \lim -> do
        peers <- getPeers
        converseToNeighbors peers sendActions (invReqDataFlowDo what tag id dt lim)
  where
    handleE e = logWarning $
        sformat ("Error sending "%stext%", id = "%build%" to neighbors: "%shown) what id e

invReqDataFlow
    :: forall tag id contents m.
    ( Message (InvOrData tag id contents)
    , Message (ReqMsg id tag)
    , Buildable id
    , MinRelayWorkMode m
    , MonadDBLimits m
    , Bi tag, Bi id
    , Bi (InvOrData tag id contents)
    , Bi (ReqMsg id tag))
    => Text -> SendActions m -> NodeId -> tag -> id -> contents -> m ()
invReqDataFlow what sendActions addr tag id dt = handleAll handleE $
    reifyMsgLimit (Proxy @(ReqMsg id tag)) $ \lim ->
        withConnectionTo sendActions addr (const (invReqDataFlowDo what tag id dt lim addr))
  where
    handleE e = logWarning $
        sformat ("Error sending "%stext%", id = "%build%" to "%shown%": "%shown) what id addr e

invReqDataFlowDo ::
    ( Message (InvOrData tag id contents)
    , Message (ReqMsg id tag)
    , Buildable id
    , MinRelayWorkMode m
    , MonadDBLimits m
    , Bi tag, Bi id
    , Bi (InvOrData tag id contents)
    , Bi (ReqMsg id tag))
    => Text -> tag -> id -> contents -> Proxy s -> NodeId
    -> ConversationActions (InvOrData tag id contents)
        (LimitedLength s (ReqMsg id tag)) m
    -> m ()
invReqDataFlowDo what tag id dt _ nodeId conv = do
    send conv $ Left $ InvMsg tag id
    recvLimited conv >>= maybe handleD replyWithData
  where
    replyWithData (ReqMsg _ _) = send conv $ Right $ DataMsg dt
    handleD = logDebug $
        sformat ("InvReqDataFlow ("%stext%"): "%shown %" closed conversation on \
                 \Inv id = "%build) what nodeId id
