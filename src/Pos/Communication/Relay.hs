-- | Framework for Inv/Req/Dat message handling

{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Communication.Relay
       ( Relay (..)
       , InvMsg (..)
       , ReqMsg (..)
       , DataMsg (..)
       , relayListeners
       , relayStubListeners
       , relayWorkers
       , RelayProxy (..)
       , InvOrData

       , invReqDataFlow
       , invReqDataFlowNeighbors
       ) where

import           Control.Concurrent.STM        (isFullTBQueue, readTBQueue, writeTBQueue)
import qualified Data.List.NonEmpty            as NE
import           Formatting                    (build, sformat, shown, stext, (%))
import           Mockable                      (Mockable, Throw, handleAll, throw, throw)
import           Node.Message                  (Message)
import           Serokell.Util.Text            (listJson)
import           Serokell.Util.Verify          (VerificationRes (..))
import           System.Wlog                   (WithLogger, logDebug, logError, logInfo,
                                                logWarning)
import           Universum

import           Pos.Binary.Class              (Bi (..))
import           Pos.Binary.Relay              ()
import           Pos.Communication.Message     (MessagePart)
import           Pos.Communication.Limits      (Limit, LimitedLengthExt (..),
                                                LimitedLength, MessageLimited (..),
                                                withLimitedLength, recvLimited,
                                                reifyMsgLimit)
import           Pos.Communication.Protocol    (ConversationActions (..), ListenerSpec,
                                                NOP, NodeId, OutSpecs, SendActions (..),
                                                WorkerSpec, listenerConv, mergeLs, worker)
import           Pos.Communication.Specs       (allOutSpecs)
import           Pos.Communication.Types.Relay (DataMsg (..), InvMsg (..), InvOrData,
                                                ReqMsg (..))
import           Pos.Communication.Util        (stubListenerConv)
import           Pos.Context                   (NodeContext (..), SomeInvMsg (..),
                                                WithNodeContext (getNodeContext),
                                                ncNodeParams, npPropagation)
import           Pos.DB.Limits                 (MonadDBLimits)
import           Pos.DHT.Model                 (DHTNode, MonadDHT (..),
                                                converseToNeighbors, converseToNode)
import           Pos.Reporting                 (reportingFatal)
import           Pos.WorkMode                  (MinWorkMode, WorkMode)

-- | Typeclass for general Inv/Req/Dat framework. It describes monads,
-- that store data described by tag, where "key" stands for node
-- identifier.
class ( Buildable tag
      , Buildable contents
      , Buildable key
      , Typeable tag
      , Typeable contents
      , Typeable key
      , Message (ReqMsg key tag)
      , Message (InvOrData tag key contents)
      , MessageLimited (DataMsg contents)
      ) => Relay m tag key contents
      | tag -> contents, contents -> tag, contents -> key, tag -> key where
    -- | Converts data to tag. Tag returned in monad `m`
    -- for only type matching reason (multiparam type classes are tricky)
    contentsToTag :: contents -> m tag

    -- | Same for key. Sometime contents has key inside already, so
    -- it's redundant to double-pass it everywhere.
    contentsToKey :: contents -> m key

    verifyInvTag :: tag -> m VerificationRes
    verifyReqTag :: tag -> m VerificationRes
    verifyDataContents :: contents -> m VerificationRes

    -- | Handle inv msg and return whether it's useful or not
    handleInv :: tag -> key -> m Bool

    -- | Handle req msg and return (Just data) in case requested data can be provided
    handleReq :: tag -> key -> m (Maybe contents)

    -- | Handle data msg and return True if message is to be propagated
    handleData :: contents -> m Bool

data RelayProxy key tag contents = RelayProxy

data RelayError = UnexpectedInv
                | UnexpectedData
  deriving (Generic, Show)

instance Exception RelayError

-- Returns useful keys.
handleInvL
    :: forall m key tag contents.
       ( Bi (InvMsg key tag)
       , Bi (ReqMsg key tag)
       , Relay m tag key contents
       , MinWorkMode m
       )
    => RelayProxy key tag contents
    -> InvMsg key tag
    -> m [key]
handleInvL proxy msg@(InvMsg{..}) =
    processMessage [] "Inventory" imTag verifyInvTag $ do
        let _ = invCatchType proxy msg
         -- TODO remove this msg when InvMsg datatype will contain only one key
        when (NE.length imKeys /= 1) $
            logWarning $ "InvMsg constains more than one key, we'll handle only first"
        let imKey = imKeys NE.!! 0
        invRes <- handleInv imTag imKey
        if invRes then
            [imKey] <$ logDebug (sformat
              ("We'll request data "%build%" for key "%build%", because it's useful")
              imTag imKey)
        else
            [] <$ logDebug (sformat
              ("Ignoring inv "%build%" for key "%build%", because it's useless")
              imTag imKey)

handleReqL
    :: forall key tag contents m .
       ( Bi (ReqMsg key tag)
       , Bi (InvOrData tag key contents)
       , Message (InvOrData tag key contents)
       , Relay m tag key contents
       , MinWorkMode m
       , MonadDBLimits m
       )
    => RelayProxy key tag contents
    -> m (ListenerSpec m, OutSpecs)
handleReqL proxy = reifyMsgLimit (Proxy @(ReqMsg key tag)) $
  \(_ :: Proxy s) -> return $ listenerConv $
    \_ __peerId ConversationActions{..} ->
    whenJustM recv $ \(withLimitedLength @s -> msg@ReqMsg {..}) -> do
      let _ = reqCatchType proxy msg
      processMessage () "Request" rmTag verifyReqTag $ do
          -- TODO remove this msg when InvMsg datatype will contain only one key
          when (NE.length rmKeys /= 1) $
              logWarning $ "ReqMsg constains more than one key, we'll handle only first"
          let rmKey = rmKeys NE.!! 0
          dtMB <- handleReq rmTag rmKey
          case dtMB of
              Nothing ->
                  logDebug $ sformat ("We don't have data "%build%" for key "%build) rmTag rmKey
              Just dt -> do
                  logDebug $ sformat ("We have data "%build%" for key "%build) rmTag rmKey
                  send $ constructDataMsg dt
  where
    constructDataMsg :: contents -> InvOrData tag key contents
    constructDataMsg = Right . DataMsg

-- Returns True if we should propagate.
handleDataL
      :: forall tag key contents ssc m .
      ( Bi (InvMsg key tag)
      , Bi (ReqMsg key tag)
      , Bi key
      , Bi tag
      , Bi (DataMsg contents)
      , MonadDHT m
      , MessagePart tag
      , MessagePart contents
      , Relay m tag key contents
      , WorkMode ssc m
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
        shouldPropagate <- npPropagation . ncNodeParams <$> getNodeContext
        if shouldPropagate then do
            tag <- contentsToTag dmContents
            let inv :: InvOrData tag key contents
                inv = Left $ InvMsg tag (one dmKey)
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

-- | Type `InvOrData` with limited length.
type InvOrDataLimitedLength s key tag contents =
    LimitedLengthExt s
        (Limit (InvMsg key tag), LimitType (DataMsg contents))
        (InvOrData tag key contents)

relayListeners
  :: forall m key tag contents ssc.
     ( MonadDHT m
     , Bi key
     , Bi tag
     , Bi (DataMsg contents)
     , Bi (ReqMsg key tag)
     , Bi (InvOrData tag key contents)
     , MessagePart contents
     , MessagePart tag
     , Relay m tag key contents
     , Mockable Throw m
     , WithLogger m
     , WorkMode ssc m
     )
  => RelayProxy key tag contents -> m ([ListenerSpec m], OutSpecs)
relayListeners proxy = mergeLs <$> sequence [handleReqL proxy, invDataListener]
  where
    invDataListener = reifyMsgLimit (Proxy @(InvOrData tag key contents)) $
      \(_ :: Proxy s) -> return $ listenerConv $ \_ __peerId
        (ConversationActions{..}::(ConversationActions
                                  (ReqMsg key tag)
                                  (InvOrDataLimitedLength s key tag contents)
                                  m)
        ) -> do
            inv' <- recv
            whenJust (withLimitedLength <$> inv') $ expectLeft $
                \inv@InvMsg{..} -> do
                    useful <- handleInvL proxy inv
                    whenJust (NE.nonEmpty useful) $ \ne -> do
                        send $ ReqMsg imTag ne
                        dt' <- recv
                        whenJust (withLimitedLength <$> dt') $ expectRight $
                            \dt@DataMsg{..} -> handleDataL proxy dt

    expectLeft call (Left msg) = call msg
    expectLeft _ (Right _)     = throw UnexpectedData

    expectRight _ (Left _)       = throw UnexpectedInv
    expectRight call (Right msg) = call msg


relayStubListeners
    :: ( WithLogger m
       , Bi (InvMsg key tag)
       , Bi (ReqMsg key tag)
       , Bi (DataMsg contents)
       , Message (InvOrData tag key contents)
       , Message (ReqMsg key tag)
       )
    => RelayProxy key tag contents -> ([ListenerSpec m], OutSpecs)
relayStubListeners p = mergeLs
    [ stubListenerConv $ invDataMsgProxy p
    , stubListenerConv $ reqMsgProxy p
    ]

invCatchType :: RelayProxy key tag contents -> InvMsg key tag -> ()
invCatchType _ _ = ()

reqCatchType :: RelayProxy key tag contents -> ReqMsg key tag -> ()
reqCatchType _ _ = ()
reqMsgProxy :: RelayProxy key tag contents
            -> Proxy (InvOrData tag key contents, ReqMsg key tag)
reqMsgProxy _ = Proxy

dataCatchType :: RelayProxy key tag contents -> DataMsg ontents -> ()
dataCatchType _ _ = ()

invDataMsgProxy :: RelayProxy key tag contents
                -> Proxy (ReqMsg key tag, InvOrData tag key contents)
invDataMsgProxy _ = Proxy


addToRelayQueue :: forall tag key contents ssc m .
                ( Bi (InvOrData tag key contents)
                , Bi (ReqMsg key tag)
                , Message (InvOrData tag key contents)
                , Message (ReqMsg key tag)
                , Buildable tag, Buildable key
                , WorkMode ssc m
                )
                => InvOrData tag key contents -> m ()
addToRelayQueue inv = do
    queue <- ncInvPropagationQueue <$> getNodeContext
    isFull <- atomically $ isFullTBQueue queue
    if isFull then
        logWarning $ "Propagation queue is full, no propagation"
    else
        atomically $ writeTBQueue queue (SomeInvMsg inv)

relayWorkers :: forall ssc m .
             ( Mockable Throw m
             , WorkMode ssc m
             , Bi NOP, Message NOP
             )
             => ([WorkerSpec m], OutSpecs)
relayWorkers =
    first (:[]) $ worker allOutSpecs $ \sendActions ->
        handleAll handleWE $ reportingFatal $ action sendActions
  where
    action sendActions = do
        queue <- ncInvPropagationQueue <$> getNodeContext
        forever $ atomically (readTBQueue queue) >>= \case
            SomeInvMsg (i@(Left (InvMsg{..}))) -> do
                logDebug $
                    sformat
                    ("Propagation data with keys: "%listJson%" and tag: "%build) imKeys imTag
                converseToNeighbors sendActions (convHandler i)
            SomeInvMsg (Right _) ->
                logWarning $ "DataMsg is contains in inv propagation queue"
    convHandler inv __peerId
        (ConversationActions{..}::
        (ConversationActions (InvOrData tag1 key1 contents1) (ReqMsg key1 tag1) m)) = send inv
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
    , MinWorkMode m
    , MonadDBLimits m
    , Bi tag, Bi id
    , Bi (InvOrData tag id contents)
    , Bi (ReqMsg id tag))
    => Text -> SendActions m -> tag -> id -> contents -> m ()
invReqDataFlowNeighbors what sendActions tag id dt = handleAll handleE $
    reifyMsgLimit (Proxy @(ReqMsg id tag)) $ \lim ->
        converseToNeighbors sendActions (invReqDataFlowDo what tag id dt lim)
  where
    handleE e = logWarning $
        sformat ("Error sending "%stext%", id = "%build%" to neighbors: "%shown) what id e

invReqDataFlow
    :: forall tag id contents m.
    ( Message (InvOrData tag id contents)
    , Message (ReqMsg id tag)
    , Buildable id
    , MinWorkMode m
    , MonadDBLimits m
    , Bi tag, Bi id
    , Bi (InvOrData tag id contents)
    , Bi (ReqMsg id tag))
    => Text -> SendActions m -> DHTNode -> tag -> id -> contents -> m ()
invReqDataFlow what sendActions addr tag id dt = handleAll handleE $
    reifyMsgLimit (Proxy @(ReqMsg id tag)) $ \lim ->
        converseToNode sendActions addr (invReqDataFlowDo what tag id dt lim)
  where
    handleE e = logWarning $
        sformat ("Error sending "%stext%", id = "%build%" to "%shown%": "%shown) what id addr e

invReqDataFlowDo ::
    ( Message (InvOrData tag id contents)
    , Message (ReqMsg id tag)
    , Buildable id
    , MinWorkMode m
    , MonadDBLimits m
    , Bi tag, Bi id
    , Bi (InvOrData tag id contents)
    , Bi (ReqMsg id tag))
    => Text -> tag -> id -> contents -> Proxy s -> NodeId
    -> ConversationActions (InvOrData tag id contents)
        (LimitedLength s (ReqMsg id tag)) m
    -> m ()
invReqDataFlowDo what tag id dt _ nodeId conv = do
    send conv $ Left $ InvMsg tag (one id)
    recvLimited conv >>= maybe handleD replyWithData
  where
    replyWithData (ReqMsg _ _) = send conv $ Right $ DataMsg dt
    handleD = logDebug $
        sformat ("InvReqDataFlow ("%stext%"): "%shown %" closed conversation on \
                 \Inv id = "%build) what nodeId id
