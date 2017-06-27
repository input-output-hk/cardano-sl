{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Framework for Inv\/Req\/Data message handling

module Pos.Communication.Relay.Logic
       ( Relay (..)
       , InvMsg (..)
       , ReqMsg (..)
       , MempoolMsg (..)
       , DataMsg (..)
       , relayListeners
       , relayMsg
       , propagateData
       , relayPropagateOut
       , InvOrData
       , handleDataDo
       , handleInvDo

       , invReqDataFlow
       , invReqDataFlowTK
       , invReqDataFlowNeighbors
       , invReqDataFlowNeighborsTK
       , dataFlow
       , InvReqDataFlowLog (..)
       ) where

import           Data.Aeson.TH                      (defaultOptions, deriveJSON)
import           Data.Proxy                         (asProxyTypeOf)
import           Data.Tagged                        (Tagged, tagWith)
import           Data.Typeable                      (typeRep)
import           Formatting                         (build, sformat, shown, stext, (%))
import           Mockable                           (MonadMockable, handleAll)
import           Node.Message.Class                 (Message)
import           System.Wlog                        (WithLogger, logDebug,
                                                     logWarning)
import           Universum

import           Pos.Binary.Class                   (Bi (..))
import           Pos.Communication.Limits.Instances ()
import           Pos.Communication.Limits.Types     (MessageLimited, recvLimited)
import           Pos.Communication.Listener         (listenerConv)
import           Pos.Communication.Protocol         (Conversation (..),
                                                     ConversationActions (..),
                                                     ListenerSpec, MkListeners, NodeId,
                                                     OutSpecs, EnqueueMsg,
                                                     constantListeners, convH,
                                                     toOutSpecs, Msg, Origin (..),
                                                     waitForConversations)
import           Pos.Communication.Relay.Class      (DataParams (..),
                                                     InvReqDataParams (..),
                                                     MempoolParams (..),
                                                     Relay (..))
import           Pos.Communication.Relay.Types      (PropagationMsg (..))
import           Pos.Communication.Relay.Util       (expectData, expectInv)
import           Pos.Communication.Types.Relay      (DataMsg (..), InvMsg (..), InvOrData,
                                                     MempoolMsg (..), ReqMsg (..))
import           Pos.DB.Class                       (MonadGState)
import           Pos.Util.TimeWarp                  (CanJsonLog (..))

type MinRelayWorkMode m =
    ( WithLogger m
    , CanJsonLog m
    , MonadMockable m
    , MonadIO m
    )

type RelayWorkMode ctx m =
    ( MinRelayWorkMode m
    )

handleReqL
    :: forall key contents m .
       ( Bi (ReqMsg key)
       , Bi (InvOrData key contents)
       , Message (InvOrData key contents)
       , Message (ReqMsg key)
       , Buildable key
       , MinRelayWorkMode m
       , MonadGState m
       )
    => (NodeId -> key -> m (Maybe contents))
    -> (ListenerSpec m, OutSpecs)
handleReqL handleReq = listenerConv $ \__ourVerInfo nodeId conv ->
    let handlingLoop = do
            mbMsg <- recvLimited conv
            whenJust mbMsg $ \ReqMsg{..} -> do
                dtMB <- handleReq nodeId rmKey
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
       , MonadGState m
       )
    => MempoolParams m
    -> [(ListenerSpec m, OutSpecs)]
handleMempoolL NoMempool = []
handleMempoolL (KeyMempool tagP handleMempool) = pure $ listenerConv $
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
    :: forall contents ctx m .
       ( Bi (DataMsg contents)
       , Message Void
       , Message (DataMsg contents)
       , Buildable contents
       , RelayWorkMode ctx m
       , MonadGState m
       , MessageLimited (DataMsg contents)
       )
    => EnqueueMsg m
    -> (Origin NodeId -> Msg)
    -> (NodeId -> contents -> m Bool)
    -> (ListenerSpec m, OutSpecs)
handleDataOnlyL enqueue mkMsg handleData = listenerConv $ \__ourVerInfo nodeId conv ->
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
       , Message (ReqMsg key)
       , Bi (InvOrData key contents)
       , Bi (ReqMsg key)
       , Message Void
       )
    => NodeId
    -> (Origin NodeId -> Msg)
    -> EnqueueMsg m
    -> (contents -> m key)
    -> (contents -> m Bool)
    -> contents
    -> m ()
handleDataDo provenance mkMsg enqueue contentsToKey handleData dmContents = do
    dmKey <- contentsToKey dmContents
    ifM (handleData dmContents)
        -- IMPORTANT that we propagate it asynchronously.
        -- enqueueMsg can do that: simply don't force the values in
        -- the resulting map.
        (void $ propagateData enqueue $ InvReqDataPM (mkMsg (OriginForward provenance)) dmKey dmContents) $
            logDebug $ sformat
                ("Ignoring data "%build%" for key "%build) dmContents dmKey

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
        enqueue msg $ \__node _ ->
            pure $ Conversation $ irdHandler key contents
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

    irdHandler
        :: Eq key1 => key1 -> contents1
        -> ConversationActions
             (InvOrData key1 contents1) (ReqMsg key1) m
        -> m ()
    irdHandler key conts conv = do
        send conv $ Left $ InvMsg key
        let whileNotK = do
              rm <- recv conv maxBound
              whenJust rm $ \ReqMsg{..} -> do
                if rmKey == key
                   then send conv $ Right $ DataMsg conts
                   else whileNotK
        whileNotK


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
  :: forall ctx m.
     ( WithLogger m
     , RelayWorkMode ctx m
     , MonadGState m
     , Message Void
     )
  => EnqueueMsg m -> Relay m -> MkListeners m
relayListenersOne enqueue (InvReqData mP irdP@InvReqDataParams{..}) =
    constantListeners $
    [handleReqL handleReq, invDataListener enqueue irdP] ++ handleMempoolL mP
relayListenersOne enqueue (Data DataParams{..}) =
    constantListeners $
    [handleDataOnlyL enqueue dataMsgType (handleDataOnly enqueue)]

relayListeners
  :: forall ctx m.
     ( WithLogger m
     , RelayWorkMode ctx m
     , MonadGState m
     , Message Void
     )
  => EnqueueMsg m -> [Relay m] -> MkListeners m
relayListeners enqueue = mconcat . map (relayListenersOne enqueue)

invDataListener
  :: forall key contents ctx m.
     ( RelayWorkMode ctx m
     , MonadGState m
     , Message (ReqMsg key)
     , Message (InvOrData key contents)
     , Bi (ReqMsg key)
     , Bi (InvOrData key contents)
     , Buildable contents
     , Buildable key
     , Eq key
     , MessageLimited (DataMsg contents)
     , Message Void
     )
  => EnqueueMsg m
  -> InvReqDataParams key contents m
  -> (ListenerSpec m, OutSpecs)
invDataListener enqueue InvReqDataParams{..} = listenerConv $ \__ourVerInfo nodeId conv ->
    let handlingLoop = do
            inv' <- recvLimited conv
            whenJust inv' $ expectInv $ \InvMsg{..} -> do
                useful <- handleInvDo (handleInv nodeId) imKey
                whenJust useful $ \ne -> do
                    send conv $ ReqMsg ne
                    dt' <- recvLimited conv
                    whenJust dt' $ expectData $ \DataMsg{..} -> do
                          handleDataDo nodeId invReqMsgType enqueue contentsToKey (handleData nodeId) dmContents
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

invReqDataFlowNeighborsTK
    :: forall key contents m.
       ( Message (InvOrData (Tagged contents key) contents)
       , Message (ReqMsg (Tagged contents key))
       , Buildable key
       , Typeable contents
       , MinRelayWorkMode m
       , MonadGState m
       , Bi (InvOrData (Tagged contents key) contents)
       , Bi (ReqMsg (Tagged contents key))
       )
    => Text -> EnqueueMsg m -> Msg -> key -> contents -> m ()
invReqDataFlowNeighborsTK what enqueue msg key dt =
    invReqDataFlowNeighbors what enqueue msg key' dt
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
       , MonadGState m
       , Bi (InvOrData (Tagged contents key) contents)
       , Bi (ReqMsg (Tagged contents key))
       )
    => Text -> EnqueueMsg m -> Msg -> key -> contents -> m ()
invReqDataFlowTK what enqueue msg key dt =
    invReqDataFlow what enqueue msg key' dt
  where
    contProxy = (const Proxy :: contents -> Proxy contents) dt
    key' = tagWith contProxy key

invReqDataFlowNeighbors
    :: forall key contents m.
       ( Message (InvOrData key contents)
       , Message (ReqMsg key)
       , Buildable key
       , MinRelayWorkMode m
       , MonadGState m
       , Bi (InvOrData key contents)
       , Bi (ReqMsg key)
       )
    => Text -> EnqueueMsg m -> Msg -> key -> contents -> m ()
invReqDataFlowNeighbors what enqueue msg key dt = handleAll handleE $
    void $ enqueue msg (\addr _ -> pure (Conversation (invReqDataFlowDo what key dt addr))) >>= waitForConversations
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
       , MonadGState m
       )
    => Text -> EnqueueMsg m -> Msg -> key -> contents -> m ()
invReqDataFlow what enqueue msg key dt = handleAll handleE $ do
    its <- enqueue msg $
        \addr _ -> pure $ Conversation $ invReqDataFlowDo what key dt addr
    void $ waitForConversations its
  where
    handleE e = logWarning $
        sformat ("Error sending "%stext%", key = "%build%": "%shown)
                what key e

invReqDataFlowDo
    :: ( Message (InvOrData key contents)
       , Message (ReqMsg key)
       , Bi (InvOrData key contents)
       , Bi (ReqMsg key)
       , Buildable key
       , MinRelayWorkMode m
       , MonadGState m
       )
    => Text
    -> key
    -> contents
    -> NodeId
    -> ConversationActions (InvOrData key contents) (ReqMsg key) m
    -> m ()
invReqDataFlowDo what key dt peer conv = do
    send conv $ Left $ InvMsg key
    recvLimited conv >>= maybe handleD replyWithData
  where
    -- TODO need to check we're asked for same key we have
    replyWithData (ReqMsg _) = send conv $ Right $ DataMsg dt
    handleD =
        logDebug $
        sformat ("InvReqDataFlow ("%stext%"): "%shown %" closed conversation on \
                 \Inv key = "%build)
                what peer key

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
    handleE e =
        logWarning $
        sformat ("Error sending "%stext%", data = "%build%": "%shown)
                what dt e
