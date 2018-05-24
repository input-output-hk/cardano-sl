{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Framework for Inv\/Req\/Data message handling

module Pos.Communication.Relay.Logic
       ( Relay (..)
       , InvMsg (..)
       , ReqMsg (..)
       , ResMsg (..)
       , MempoolMsg (..)
       , DataMsg (..)
       , InvOrData
       , ReqOrRes
       , relayListeners
       , relayMsg
       , propagateData
       , relayPropagateOut
       , handleDataDo
       , handleInvDo

       , invReqDataFlow
       , invReqDataFlowTK
       , dataFlow
       , InvReqDataFlowLog (..)
       ) where

import           Control.Exception (throwIO)
import           Control.Exception.Safe (handleAny, try)
import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Data.Proxy (asProxyTypeOf)
import           Data.Tagged (Tagged, tagWith)
import           Data.Typeable (typeRep)
import           Formatting (build, sformat, shown, stext, (%))
import qualified Network.Broadcast.OutboundQueue as OQ
import           Node.Message.Class (Message)
import           Universum

import           Pos.Binary.Class (Bi (..))
import           Pos.Communication.Limits.Instances (mlResMsg, mlReqMsg, mlInvMsg,
                                                     mlDataMsg, mlMempoolMsg)
import           Pos.Communication.Limits.Types (Limit, mlEither)
import           Pos.Communication.Listener (listenerConv)
import           Pos.Communication.Protocol (Conversation (..), ConversationActions (..),
                                             EnqueueMsg, ListenerSpec, MkListeners, Msg, NodeId,
                                             Origin (..), OutSpecs, constantListeners, convH,
                                             toOutSpecs, waitForDequeues, waitForConversations,
                                             recvLimited)
import           Pos.Communication.Relay.Class (DataParams (..), InvReqDataParams (..),
                                                MempoolParams (..), Relay (..))
import           Pos.Communication.Relay.Types (PropagationMsg (..))
import           Pos.Communication.Relay.Util (expectData, expectInv)
import           Pos.Communication.Types.Relay (DataMsg (..), InvMsg (..), InvOrData,
                                                MempoolMsg (..), ReqMsg (..), ReqOrRes, ResMsg (..))
import           Pos.Network.Types (Bucket)
import           Pos.Util.Trace (Trace, Severity (..), traceWith)

data InvReqCommunicationException =
      UnexpectedRequest
    | UnexpectedResponse
    | UnexpectedEnd
    | MismatchedKey
    deriving (Show)

instance Exception InvReqCommunicationException

handleReqL
    :: forall pack key contents .
       ( Bi (ReqMsg key)
       , Bi (InvOrData key contents)
       , Message (InvOrData key contents)
       , Message (ReqMsg key)
       , Buildable key
       )
    => Trace IO (Severity, Text)
    -> OQ.OutboundQ pack NodeId Bucket
    -> (NodeId -> key -> IO (Maybe contents))
    -> (ListenerSpec, OutSpecs)
handleReqL logTrace oq handleReq = listenerConv logTrace oq $ \__ourVerInfo nodeId conv ->
    let handlingLoop = do
            mbMsg <- recvLimited conv mlReqMsg
            case mbMsg of
                Just (ReqMsg (Just key)) -> do
                    dtMB <- handleReq nodeId key
                    case dtMB of
                        Nothing -> logNoData key
                        Just dt -> logHaveData key >> send conv (constructDataMsg dt)
                    handlingLoop
                _ -> return ()
    in handlingLoop
  where
    constructDataMsg :: contents -> InvOrData key contents
    constructDataMsg = Right . DataMsg
    logNoData rmKey = traceWith logTrace (Debug, sformat
        ("We don't have data for key "%build)
        rmKey)
    logHaveData rmKey= traceWith logTrace (Debug, sformat
        ("We have data for key "%build)
        rmKey)

handleMempoolL
    :: forall pack.
       Trace IO (Severity, Text)
    -> OQ.OutboundQ pack NodeId Bucket
    -> MempoolParams
    -> [(ListenerSpec, OutSpecs)]
handleMempoolL _ _ NoMempool = []
handleMempoolL logTrace oq (KeyMempool tagP handleMempool) = pure $ listenerConv logTrace oq $
    \__ourVerInfo __nodeId conv -> do
        mbMsg <- recvLimited conv mlMempoolMsg
        whenJust mbMsg $ \msg@MempoolMsg -> do
            let _ = msg `asProxyTypeOf` mmP
            res <- handleMempool
            case nonEmpty res of
                Nothing ->
                    traceWith logTrace (Debug, sformat
                        ("We don't have mempool data "%shown) (typeRep tagP))
                Just xs -> do
                    traceWith logTrace (Debug, sformat ("We have mempool data "%shown) (typeRep tagP))
                    mapM_ (send conv . InvMsg) xs
  where
    mmP = (const Proxy :: Proxy tag -> Proxy (MempoolMsg tag)) tagP

handleDataOnlyL
    :: forall pack contents.
       ( Bi (DataMsg contents)
       , Message Void
       , Message (DataMsg contents)
       , Buildable contents
       )
    => Trace IO (Severity, Text)
    -> OQ.OutboundQ pack NodeId Bucket
    -> EnqueueMsg
    -> (Origin NodeId -> Msg)
    -> IO (Limit contents)
    -> (NodeId -> contents -> IO Bool)
    -> (ListenerSpec, OutSpecs)
handleDataOnlyL logTrace oq enqueue mkMsg mkLimit handleData = listenerConv logTrace oq $ \__ourVerInfo nodeId conv ->
    -- First binding is to inform GHC that the send type is Void.
    let msg :: Msg
        msg = mkMsg (OriginForward nodeId)
        _ = send conv :: Void -> IO ()
        handlingLoop = do
            lim <- mkLimit
            mbMsg <- recvLimited conv (mlDataMsg lim)
            whenJust mbMsg $ \DataMsg{..} -> do
                ifM (handleData nodeId dmContents)
                    (void $ propagateData logTrace enqueue $ DataOnlyPM msg dmContents)
                    (logUseless dmContents)
                handlingLoop
    in handlingLoop
  where
    logUseless dmContents = traceWith logTrace (Warning, sformat
        ("Ignoring data "%build) dmContents)

handleDataDo
    :: forall key contents.
       ( Buildable key
       , Eq key
       , Buildable contents
       , Message (InvOrData key contents)
       , Message (ReqOrRes key)
       , Bi (InvOrData key contents)
       , Bi (ReqOrRes key)
       , Message Void
       )
    => Trace IO (Severity, Text)
    -> NodeId
    -> (Origin NodeId -> Msg)
    -> EnqueueMsg
    -> (contents -> IO key)
    -> (contents -> IO Bool)
    -> contents
    -> IO (ResMsg key)
handleDataDo logTrace provenance mkMsg enqueue contentsToKey handleData dmContents = do
    dmKey <- contentsToKey dmContents
    ifM (handleData dmContents)
        -- IMPORTANT that we propagate it asynchronously.
        -- enqueueMsg can do that: simply don't force the values in
        -- the resulting map.
        (ResMsg dmKey True <$ propagateData logTrace enqueue (InvReqDataPM (mkMsg (OriginForward provenance)) dmKey dmContents))
        (ResMsg dmKey False <$ (traceWith logTrace (Debug, sformat ("Ignoring data "%build%" for key "%build) dmContents dmKey)))

-- | Synchronously propagate data.
relayMsg
    :: ( Message Void )
    => Trace IO (Severity, Text)
    -> EnqueueMsg
    -> PropagationMsg
    -> IO ()
relayMsg logTrace enqueue pm = void $ propagateData logTrace enqueue pm >>= waitForConversations

-- | Asynchronously propagate data.
propagateData
    :: ( Message Void )
    => Trace IO (Severity, Text)
    -> EnqueueMsg
    -> PropagationMsg
    -> IO (Map NodeId (IO ()))
propagateData logTrace enqueue pm = waitForDequeues <$> case pm of
    InvReqDataPM msg key contents -> do
        traceWith logTrace (Debug, sformat
            ("Propagation data with key: "%build) key)
        enqueue msg $ \peer _ ->
            pure $ Conversation $ (void <$> invReqDataFlowDo logTrace "propagation" key contents peer)
    DataOnlyPM msg contents -> do
        traceWith logTrace (Debug, sformat
            ("Propagation data: "%build) contents)
        enqueue msg $ \__node _ ->
            pure $ Conversation $ doHandler contents

  where

    doHandler
        :: contents1
        -> ConversationActions
             (DataMsg contents1) Void
        -> IO ()
    doHandler contents conv = send conv $ DataMsg contents

handleInvDo
    :: forall key.
       ( Buildable key)
    => Trace IO (Severity, Text)
    -> (key -> IO Bool)
    -> key
    -> IO (Maybe key)
handleInvDo logTrace handleInv imKey =
    ifM (handleInv imKey)
        (Just imKey <$ logUseful)
        (Nothing <$ logUseless)
  where
    logUseless = traceWith logTrace (Debug, sformat
        ("Ignoring inv for key "%build%", because it's useless")
        imKey)
    logUseful = traceWith logTrace (Debug, sformat
        ("We'll request data for key "%build%", because it's useful")
        imKey)

relayListenersOne
    :: forall pack.
       ( Message Void )
    => Trace IO (Severity, Text)
    -> OQ.OutboundQ pack NodeId Bucket
    -> EnqueueMsg
    -> Relay
    -> MkListeners
relayListenersOne logTrace oq enqueue (InvReqData mP irdP@InvReqDataParams{..}) =
    constantListeners $
    [handleReqL logTrace oq handleReq, invDataListener logTrace oq enqueue irdP] ++ handleMempoolL logTrace oq mP
relayListenersOne logTrace oq enqueue (Data DataParams{..}) =
    constantListeners $
    [handleDataOnlyL logTrace oq enqueue dataMsgType dpMkLimit (handleDataOnly enqueue)]

relayListeners
    :: forall pack.
       ( Message Void )
    => Trace IO (Severity, Text)
    -> OQ.OutboundQ pack NodeId Bucket
    -> EnqueueMsg
    -> [Relay]
    -> MkListeners
relayListeners logTrace oq enqueue = mconcat . map (relayListenersOne logTrace oq enqueue)

invDataListener
  :: forall pack key contents.
     ( Message (ReqOrRes key)
     , Message (InvOrData key contents)
     , Bi (ReqOrRes key)
     , Bi (InvOrData key contents)
     , Buildable contents
     , Buildable key
     , Eq key
     , Message Void
     )
  => Trace IO (Severity, Text)
  -> OQ.OutboundQ pack NodeId Bucket
  -> EnqueueMsg
  -> InvReqDataParams key contents
  -> (ListenerSpec, OutSpecs)
invDataListener logTrace oq enqueue InvReqDataParams{..} = listenerConv logTrace oq $ \__ourVerInfo nodeId conv ->
    let handlingLoop = do
            lim <- irdpMkLimit
            inv' <- recvLimited conv (mlEither mlInvMsg (mlDataMsg lim))
            whenJust inv' $ expectInv $ \InvMsg{..} -> do
                useful <- handleInvDo logTrace (handleInv nodeId) imKey
                case useful of
                    Nothing -> send conv (Left (ReqMsg Nothing))
                    Just ne -> do
                        send conv $ Left (ReqMsg (Just ne))
                        dt' <- recvLimited conv (mlEither mlInvMsg (mlDataMsg lim))
                        whenJust dt' $ expectData $ \DataMsg{..} -> do
                              res <- handleDataDo logTrace nodeId invReqMsgType enqueue contentsToKey (handleData nodeId) dmContents
                              send conv $ Right res
                              -- handlingLoop

                              -- TODO CSL-1148 Improve relaing: support multiple data
                              -- Need to receive Inv and Data messages simultaneously
                              -- Maintain state of sent Reqs
                              -- And check data we are sent is what we expect (currently not)
    in handlingLoop

relayPropagateOut :: Message Void => [Relay] -> OutSpecs
relayPropagateOut = mconcat . map propagateOutImpl

propagateOutImpl :: Message Void => Relay -> OutSpecs
propagateOutImpl (InvReqData _ irdp) = toOutSpecs
      [ convH invProxy reqResProxy ]
  where
    invProxy    = (const Proxy :: InvReqDataParams key contents
                               -> Proxy (InvOrData key contents)) irdp
    reqResProxy = (const Proxy :: InvReqDataParams key contents
                               -> Proxy (ReqOrRes key)) irdp
propagateOutImpl (Data dp) = toOutSpecs
      [ convH dataProxy (Proxy @Void)
      ]
  where
    dataProxy = (const Proxy :: DataParams contents
                             -> Proxy (DataMsg contents)) dp

invReqDataFlowDo
    :: ( Buildable key
       , Eq key
       )
    => Trace IO (Severity, Text)
    -> Text
    -> key
    -> contents
    -> NodeId
    -> ConversationActions (InvOrData key contents) (ReqOrRes key)
    -> IO (Maybe (ResMsg key))
invReqDataFlowDo logTrace what key dt peer conv = do
    send conv $ Left $ InvMsg key
    it <- recvLimited conv (mlEither mlReqMsg mlResMsg)
    maybe handleD replyWithData it
  where
    replyWithData (Left (ReqMsg (Just key'))) = do
        -- Stop if the peer sends the wrong key. Basically a protocol error.
        unless (key' == key) (throwIO MismatchedKey)
        send conv $ Right $ DataMsg dt
        it <- recvLimited conv (mlEither mlReqMsg mlResMsg)
        maybe handleD checkResponse it
    -- The peer indicated that he doesn't want the data.
    replyWithData (Left (ReqMsg Nothing)) = return Nothing
    -- The peer sent a ResMsg where a ReqMsg was expected.
    replyWithData (Right (ResMsg _ _)) = do
        traceWith logTrace (Error,
            sformat ("InvReqDataFlow ("%stext%"): "%shown %" unexpected response")
                    what peer)
        throwIO UnexpectedResponse

    checkResponse (Right resMsg) = return (Just resMsg)
    -- The peer sent a ReqMsg where a ResMsg was expected.
    checkResponse (Left (ReqMsg _)) = do
        traceWith logTrace (Error,
            sformat ("InvReqDataFlow ("%stext%"): "%shown %" unexpected request")
                    what peer)
        throwIO UnexpectedRequest

    handleD = do
        traceWith logTrace (Error,
            sformat ("InvReqDataFlow ("%stext%"): "%shown %" closed conversation on \
                     \Inv key = "%build)
                    what peer key)
        throwIO UnexpectedEnd

dataFlow
    :: forall contents.
       ( Message (DataMsg contents)
       , Bi (DataMsg contents)
       , Buildable contents
       , Message Void
       )
    => Trace IO (Severity, Text) -> Text -> EnqueueMsg -> Msg -> contents -> IO ()
dataFlow logTrace what enqueue msg dt = handleAny handleE $ do
    its <- enqueue msg $
        \_ _ -> pure $ Conversation $ \(conv :: ConversationActions (DataMsg contents) Void) ->
            send conv $ DataMsg dt
    void $ waitForConversations (waitForDequeues its)
  where
    -- TODO: is this function really special that it wants to catch
    -- all exceptions and log them instead of letting higher-level
    -- code to do it?
    -- FIXME: are we sure we don't want to propagate exception to caller???
    -- Fortunatelly, it's used only in auxx, so I don't care much.
    -- @gromak
    handleE e =
        traceWith logTrace (Warning, sformat ("Error sending "%stext%", data = "%build%": "%shown) what dt e)

----------------------------------------------------------------------------
-- Helpers for Communication.Methods
----------------------------------------------------------------------------

data InvReqDataFlowLog =
      InvReqAccepted
        { invReqStart    :: !Integer
        , invReqReceived :: !Integer
        , invReqSent     :: !Integer
        , invReqClosed   :: !Integer
        }
    | InvReqRejected
        { invReqStart    :: !Integer
        , invReqReceived :: !Integer
        }
    | InvReqException !Text
    deriving Show

$(deriveJSON defaultOptions ''InvReqDataFlowLog)

invReqDataFlowTK
    :: forall key contents.
       ( Message (InvOrData (Tagged contents key) contents)
       , Message (ReqOrRes (Tagged contents key))
       , Buildable key
       , Typeable contents
       , Bi (InvOrData (Tagged contents key) contents)
       , Bi (ReqOrRes (Tagged contents key))
       , Eq key
       )
    => Trace IO (Severity, Text)
    -> Text
    -> EnqueueMsg
    -> Msg
    -> key
    -> contents
    -> IO (Map NodeId (Either SomeException (Maybe (ResMsg (Tagged contents key)))))
invReqDataFlowTK logTrace what enqueue msg key dt =
    invReqDataFlow logTrace what enqueue msg key' dt
  where
    contProxy = (const Proxy :: contents -> Proxy contents) dt
    key' = tagWith contProxy key

-- | Do an Inv/Req/Data/Res conversation (peers determined by the 'EnqueueMsg'
-- argument) and wait for the results.
-- This will wait for all conversations to finish. Exceptions in the conversations
-- themselves are caught and returned as Left. If the peer did not ask for the
-- data, then Right Nothing is given, otherwise their response to the data is
-- given.
invReqDataFlow
    :: forall key contents.
       ( Message (InvOrData key contents)
       , Message (ReqOrRes key)
       , Bi (InvOrData key contents)
       , Bi (ReqOrRes key)
       , Buildable key
       , Eq key
       )
    => Trace IO (Severity, Text)
    -> Text
    -> EnqueueMsg
    -> Msg
    -> key
    -> contents
    -> IO (Map NodeId (Either SomeException (Maybe (ResMsg key))))
invReqDataFlow logTrace what enqueue msg key dt = handleAny handleE $ do
    its <- enqueue msg $
        \addr _ -> pure $ Conversation $ invReqDataFlowDo logTrace what key dt addr
    waitForConversations (try <$> waitForDequeues its)
  where
    -- TODO: is this function really special that it wants to catch
    -- all exceptions and log them instead of letting higher-level
    -- code to do it?
    -- Anyway, 'reportOrLog' is not used here, because exception is rethrown.
    -- @gromak
    handleE e = do
        traceWith logTrace (Warning,
            sformat ("Error sending "%stext%", key = "%build%": "%shown)
                what key e)
        throwIO e
