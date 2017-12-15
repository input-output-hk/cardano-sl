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

       , MinRelayWorkMode
       ) where

import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Data.Proxy (asProxyTypeOf)
import           Data.Tagged (Tagged, tagWith)
import           Data.Typeable (typeRep)
import           Formatting (build, sformat, shown, stext, (%))
import           Mockable (MonadMockable, handleAll, throw, try)
import qualified Network.Broadcast.OutboundQueue as OQ
import           Node.Message.Class (Message)
import           System.Wlog (WithLogger, logDebug, logError, logWarning)
import           Universum

import           Pos.Binary.Class (Bi (..))
import           Pos.Communication.Limits.Instances ()
import           Pos.Communication.Limits.Types (MessageLimited, recvLimited)
import           Pos.Communication.Listener (listenerConv)
import           Pos.Communication.Protocol (Conversation (..), ConversationActions (..),
                                             EnqueueMsg, ListenerSpec, MkListeners, Msg, NodeId,
                                             Origin (..), OutSpecs, constantListeners, convH,
                                             toOutSpecs, waitForConversations)
import           Pos.Communication.Relay.Class (DataParams (..), InvReqDataParams (..),
                                                MempoolParams (..), Relay (..))
import           Pos.Communication.Relay.Types (PropagationMsg (..))
import           Pos.Communication.Relay.Util (expectData, expectInv)
import           Pos.Communication.Types.Relay (DataMsg (..), InvMsg (..), InvOrData,
                                                MempoolMsg (..), ReqMsg (..), ReqOrRes, ResMsg (..))
import           Pos.Infra.Configuration (HasInfraConfiguration)
import           Pos.Network.Types (Bucket)
import           Pos.Util.TimeWarp (CanJsonLog (..))

type MinRelayWorkMode m =
    ( WithLogger m
    , CanJsonLog m
    , MonadMockable m
    , MonadIO m
    , HasInfraConfiguration
    )

type RelayWorkMode ctx m =
    ( MinRelayWorkMode m
    )

data InvReqCommunicationException =
      UnexpectedRequest
    | UnexpectedResponse
    | UnexpectedEnd
    | MismatchedKey
    deriving (Show)

instance Exception InvReqCommunicationException

handleReqL
    :: forall pack key contents m .
       ( Bi (ReqMsg key)
       , Bi (InvOrData key contents)
       , Message (InvOrData key contents)
       , Message (ReqMsg key)
       , Buildable key
       , MinRelayWorkMode m
       )
    => OQ.OutboundQ pack NodeId Bucket
    -> (NodeId -> key -> m (Maybe contents))
    -> (ListenerSpec m, OutSpecs)
handleReqL oq handleReq = listenerConv oq $ \__ourVerInfo nodeId conv ->
    let handlingLoop = do
            mbMsg <- recvLimited conv
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
    logNoData rmKey = logDebug $ sformat
        ("We don't have data for key "%build)
        rmKey
    logHaveData rmKey= logDebug $ sformat
        ("We have data for key "%build)
        rmKey

handleMempoolL
    :: forall pack m.
       ( MinRelayWorkMode m
       )
    => OQ.OutboundQ pack NodeId Bucket
    -> MempoolParams m
    -> [(ListenerSpec m, OutSpecs)]
handleMempoolL _ NoMempool = []
handleMempoolL oq (KeyMempool tagP handleMempool) = pure $ listenerConv oq $
    \__ourVerInfo __nodeId conv -> do
        mbMsg <- recvLimited conv
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
  where
    mmP = (const Proxy :: Proxy tag -> Proxy (MempoolMsg tag)) tagP

handleDataOnlyL
    :: forall pack contents ctx m .
       ( Bi (DataMsg contents)
       , Message Void
       , Message (DataMsg contents)
       , Buildable contents
       , RelayWorkMode ctx m
       , MessageLimited (DataMsg contents) m
       )
    => OQ.OutboundQ pack NodeId Bucket
    -> EnqueueMsg m
    -> (Origin NodeId -> Msg)
    -> (NodeId -> contents -> m Bool)
    -> (ListenerSpec m, OutSpecs)
handleDataOnlyL oq enqueue mkMsg handleData = listenerConv oq $ \__ourVerInfo nodeId conv ->
    -- First binding is to inform GHC that the send type is Void.
    let msg :: Msg
        msg = mkMsg (OriginForward nodeId)
        _ = send conv :: Void -> m ()
        handlingLoop = do
            mbMsg <- recvLimited conv
            whenJust mbMsg $ \DataMsg{..} -> do
                ifM (handleData nodeId dmContents)
                    (void $ propagateData enqueue $ DataOnlyPM msg dmContents)
                    (logUseless dmContents)
                handlingLoop
    in handlingLoop
  where
    logUseless dmContents = logWarning $ sformat
        ("Ignoring data "%build) dmContents

handleDataDo
    :: forall key contents ctx m .
       ( RelayWorkMode ctx m
       , Buildable key
       , Eq key
       , Buildable contents
       , Message (InvOrData key contents)
       , Message (ReqOrRes key)
       , Bi (InvOrData key contents)
       , Bi (ReqOrRes key)
       , Message Void
       )
    => NodeId
    -> (Origin NodeId -> Msg)
    -> EnqueueMsg m
    -> (contents -> m key)
    -> (contents -> m Bool)
    -> contents
    -> m (ResMsg key)
handleDataDo provenance mkMsg enqueue contentsToKey handleData dmContents = do
    dmKey <- contentsToKey dmContents
    ifM (handleData dmContents)
        -- IMPORTANT that we propagate it asynchronously.
        -- enqueueMsg can do that: simply don't force the values in
        -- the resulting map.
        (ResMsg dmKey True <$ propagateData enqueue (InvReqDataPM (mkMsg (OriginForward provenance)) dmKey dmContents))
        (ResMsg dmKey False <$ logDebug (sformat ("Ignoring data "%build%" for key "%build) dmContents dmKey))

-- | Synchronously propagate data.
relayMsg
    :: ( RelayWorkMode ctx m
       , Message Void
       )
    => EnqueueMsg m
    -> PropagationMsg
    -> m ()
relayMsg enqueue pm = void $ propagateData enqueue pm >>= waitForConversations

-- | Asynchronously propagate data.
propagateData
    :: forall ctx m.
       ( RelayWorkMode ctx m
       , Message Void
       )
    => EnqueueMsg m
    -> PropagationMsg
    -> m (Map NodeId (m ()))
propagateData enqueue pm = case pm of
    InvReqDataPM msg key contents -> do
        logDebug $ sformat
            ("Propagation data with key: "%build) key
        enqueue msg $ \peer _ ->
            pure $ Conversation $ (void <$> invReqDataFlowDo "propagation" key contents peer)
    DataOnlyPM msg contents -> do
        logDebug $ sformat
            ("Propagation data: "%build) contents
        enqueue msg $ \__node _ ->
            pure $ Conversation $ doHandler contents

  where

    doHandler
        :: contents1
        -> ConversationActions
             (DataMsg contents1) Void m
        -> m ()
    doHandler contents conv = send conv $ DataMsg contents

handleInvDo
    :: forall key ctx m .
       ( RelayWorkMode ctx m
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
  :: forall pack ctx m.
     ( RelayWorkMode ctx m
     , Message Void
     )
  => OQ.OutboundQ pack NodeId Bucket -> EnqueueMsg m -> Relay m -> MkListeners m
relayListenersOne oq enqueue (InvReqData mP irdP@InvReqDataParams{..}) =
    constantListeners $
    [handleReqL oq handleReq, invDataListener oq enqueue irdP] ++ handleMempoolL oq mP
relayListenersOne oq enqueue (Data DataParams{..}) =
    constantListeners $
    [handleDataOnlyL oq enqueue dataMsgType (handleDataOnly enqueue)]

relayListeners
  :: forall pack ctx m.
     ( WithLogger m
     , RelayWorkMode ctx m
     , Message Void
     )
  => OQ.OutboundQ pack NodeId Bucket -> EnqueueMsg m -> [Relay m] -> MkListeners m
relayListeners oq enqueue = mconcat . map (relayListenersOne oq enqueue)

invDataListener
  :: forall pack key contents ctx m.
     ( RelayWorkMode ctx m
     , Message (ReqOrRes key)
     , Message (InvOrData key contents)
     , Bi (ReqOrRes key)
     , Bi (InvOrData key contents)
     , Buildable contents
     , Buildable key
     , Eq key
     , MessageLimited (DataMsg contents) m
     , Message Void
     )
  => OQ.OutboundQ pack NodeId Bucket
  -> EnqueueMsg m
  -> InvReqDataParams key contents m
  -> (ListenerSpec m, OutSpecs)
invDataListener oq enqueue InvReqDataParams{..} = listenerConv oq $ \__ourVerInfo nodeId conv ->
    let handlingLoop = do
            inv' <- recvLimited conv
            whenJust inv' $ expectInv $ \InvMsg{..} -> do
                useful <- handleInvDo (handleInv nodeId) imKey
                case useful of
                    Nothing -> send conv (Left (ReqMsg Nothing))
                    Just ne -> do
                        send conv $ Left (ReqMsg (Just ne))
                        dt' <- recvLimited conv
                        whenJust dt' $ expectData $ \DataMsg{..} -> do
                              res <- handleDataDo nodeId invReqMsgType enqueue contentsToKey (handleData nodeId) dmContents
                              send conv $ Right res
                              -- handlingLoop

                              -- TODO CSL-1148 Improve relaing: support multiple data
                              -- Need to receive Inv and Data messages simultaneously
                              -- Maintain state of sent Reqs
                              -- And check data we are sent is what we expect (currently not)
    in handlingLoop

relayPropagateOut :: Message Void => [Relay m] -> OutSpecs
relayPropagateOut = mconcat . map propagateOutImpl

propagateOutImpl :: Message Void => Relay m -> OutSpecs
propagateOutImpl (InvReqData _ irdp) = toOutSpecs
      [ convH invProxy reqResProxy ]
  where
    invProxy    = (const Proxy :: InvReqDataParams key contents m
                               -> Proxy (InvOrData key contents)) irdp
    reqResProxy = (const Proxy :: InvReqDataParams key contents m
                               -> Proxy (ReqOrRes key)) irdp
propagateOutImpl (Data dp) = toOutSpecs
      [ convH dataProxy (Proxy @Void)
      ]
  where
    dataProxy = (const Proxy :: DataParams contents m
                            -> Proxy (DataMsg contents)) dp

invReqDataFlowDo
    :: ( Buildable key
       , MinRelayWorkMode m
       , MessageLimited (ReqOrRes key) m
       , Eq key
       )
    => Text
    -> key
    -> contents
    -> NodeId
    -> ConversationActions (InvOrData key contents) (ReqOrRes key) m
    -> m (Maybe (ResMsg key))
invReqDataFlowDo what key dt peer conv = do
    send conv $ Left $ InvMsg key
    it <- recvLimited conv
    maybe handleD replyWithData it
  where
    replyWithData (Left (ReqMsg (Just key'))) = do
        -- Stop if the peer sends the wrong key. Basically a protocol error.
        unless (key' == key) (throw MismatchedKey)
        send conv $ Right $ DataMsg dt
        it <- recvLimited conv
        maybe handleD checkResponse it
    -- The peer indicated that he doesn't want the data.
    replyWithData (Left (ReqMsg Nothing)) = return Nothing
    -- The peer sent a ResMsg where a ReqMsg was expected.
    replyWithData (Right (ResMsg _ _)) = do
        logError $
            sformat ("InvReqDataFlow ("%stext%"): "%shown %" unexpected response")
                    what peer
        throw UnexpectedResponse

    checkResponse (Right resMsg) = return (Just resMsg)
    -- The peer sent a ReqMsg where a ResMsg was expected.
    checkResponse (Left (ReqMsg _)) = do
        logError $
            sformat ("InvReqDataFlow ("%stext%"): "%shown %" unexpected request")
                    what peer
        throw UnexpectedRequest

    handleD = do
        logError $
            sformat ("InvReqDataFlow ("%stext%"): "%shown %" closed conversation on \
                     \Inv key = "%build)
                    what peer key
        throw UnexpectedEnd

dataFlow
    :: forall contents m.
       ( Message (DataMsg contents)
       , Bi (DataMsg contents)
       , Buildable contents
       , MinRelayWorkMode m
       , Message Void
       )
    => Text -> EnqueueMsg m -> Msg -> contents -> m ()
dataFlow what enqueue msg dt = handleAll handleE $ do
    its <- enqueue msg $
        \_ _ -> pure $ Conversation $ \(conv :: ConversationActions (DataMsg contents) Void m) ->
            send conv $ DataMsg dt
    void $ waitForConversations its
  where
    -- TODO: is this function really special that it wants to catch
    -- all exceptions and log them instead of letting higher-level
    -- code to do it?
    -- FIXME: are we sure we don't want to propagate exception to caller???
    -- Fortunatelly, it's used only in auxx, so I don't care much.
    -- @gromak
    handleE e =
        logWarning $
        sformat ("Error sending "%stext%", data = "%build%": "%shown)
                what dt e

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
    :: forall key contents m.
       ( Message (InvOrData (Tagged contents key) contents)
       , Message (ReqOrRes (Tagged contents key))
       , Buildable key
       , Typeable contents
       , MinRelayWorkMode m
       , Bi (InvOrData (Tagged contents key) contents)
       , Bi (ReqOrRes (Tagged contents key))
       , Eq key
       )
    => Text
    -> EnqueueMsg m
    -> Msg
    -> key
    -> contents
    -> m (Map NodeId (Either SomeException (Maybe (ResMsg (Tagged contents key)))))
invReqDataFlowTK what enqueue msg key dt =
    invReqDataFlow what enqueue msg key' dt
  where
    contProxy = (const Proxy :: contents -> Proxy contents) dt
    key' = tagWith contProxy key

-- | Do an Inv/Req/Data/Res conversation (peers determined by the 'EnqueueMsg m'
-- argument) and wait for the results.
-- This will wait for all conversations to finish. Exceptions in the conversations
-- themselves are caught and returned as Left. If the peer did not ask for the
-- data, then Right Nothing is given, otherwise their response to the data is
-- given.
invReqDataFlow
    :: forall key contents m.
       ( Message (InvOrData key contents)
       , Message (ReqOrRes key)
       , Bi (InvOrData key contents)
       , Bi (ReqOrRes key)
       , Buildable key
       , MinRelayWorkMode m
       , Eq key
       )
    => Text
    -> EnqueueMsg m
    -> Msg
    -> key
    -> contents
    -> m (Map NodeId (Either SomeException (Maybe (ResMsg key))))
invReqDataFlow what enqueue msg key dt = handleAll handleE $ do
    its <- enqueue msg $
        \addr _ -> pure $ Conversation $ invReqDataFlowDo what key dt addr
    waitForConversations (fmap try its)
  where
    -- TODO: is this function really special that it wants to catch
    -- all exceptions and log them instead of letting higher-level
    -- code to do it?
    -- Anyway, 'reportOrLog' is not used here, because exception is rethrown.
    -- @gromak
    handleE e = do
        logWarning $
            sformat ("Error sending "%stext%", key = "%build%": "%shown)
                what key e
        throw e
