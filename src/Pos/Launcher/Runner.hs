{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Runners in various modes.

module Pos.Launcher.Runner
       ( -- * High level runners
         runRealMode
       , runRealBasedMode

       -- * Exported for custom usage in CLI utils
       , runServer

       , OQ
       , initQueue
       ) where

import qualified Data.Map                        as M
import           Universum                       hiding (bracket)

import           Control.Monad.Fix               (MonadFix)
import qualified Control.Monad.Reader            as Mtl
import           Formatting                      (build, sformat, (%), shown)
import           Mockable                        (MonadMockable, Production (..), bracket,
                                                  killThread, Throw, throw, Mockable,
                                                  async, cancel)
import qualified Network.Broadcast.OutboundQueue as OQ
import           Network.Broadcast.OutboundQueue.Types (FormatMsg (..))
import           Node                            (Node, NodeAction (..), NodeEndPoint,
                                                  ReceiveDelay, Statistics,
                                                  defaultNodeEnvironment,
                                                  noReceiveDelay, node,
                                                  simpleNodeEndPoint)
import qualified Node.Conversation               as N (Converse, Conversation, converseWith)
import           Node.Util.Monitor               (setupMonitor, stopMonitor)
import qualified System.Metrics                  as Metrics
import           System.Random                   (newStdGen)
import qualified System.Remote.Monitoring        as Monitoring
import qualified System.Remote.Monitoring.Statsd as Monitoring
import           System.Wlog                     (WithLogger, logInfo)

import           Pos.Binary                      ()
import           Pos.Communication               (ActionSpec (..), BiP (..), InSpecs (..),
                                                  MkListeners (..), OutSpecs (..),
                                                  VerInfo (..), allListeners, Msg,
                                                  PeerData, PackingType,
                                                  hoistSendActions, makeSendActions,
                                                  WithPeerState, SendActions,
                                                  makeEnqueueMsg, EnqueueMsg)
import qualified Pos.Constants                   as Const
import           Pos.Context                     (NodeContext (..))
import           Pos.DHT.Real                    (foreverRejoinNetwork)
import           Pos.Discovery                   (DiscoveryContextSum (..))
import           Pos.Launcher.Param              (BaseParams (..), LoggingParams (..),
                                                  NodeParams (..))
import           Pos.Launcher.Resource           (NodeResources (..), hoistNodeResources)
import           Pos.Network.Types               (NetworkConfig (..), NodeId)
import           Pos.Security                    (SecurityWorkersClass)
import           Pos.Ssc.Class                   (SscConstraint)
import           Pos.Statistics                  (EkgParams (..), StatsdParams (..))
import           Pos.Util.JsonLog                (JsonLogConfig (..),
                                                  jsonLogConfigFromHandle)
import           Pos.WorkMode                    (RealMode, RealModeContext (..),
                                                  WorkMode)

----------------------------------------------------------------------------
-- High level runners
----------------------------------------------------------------------------

-- | Run activity in 'RealMode'.
runRealMode
    :: forall ssc a.
       (SscConstraint ssc, SecurityWorkersClass ssc)
    => NodeResources ssc (RealMode ssc)
    -> (ActionSpec (RealMode ssc) a, OutSpecs)
    -> Production a
runRealMode = runRealBasedMode identity identity

-- | Run activity in something convertible to 'RealMode' and back.
runRealBasedMode
    :: forall ssc ctx m a.
       (SscConstraint ssc, SecurityWorkersClass ssc, WorkMode ssc ctx m)
    => (forall b. m b -> RealMode ssc b)
    -> (forall b. RealMode ssc b -> m b)
    -> NodeResources ssc m
    -> (ActionSpec m a, OutSpecs)
    -> Production a
runRealBasedMode unwrap wrap nr@NodeResources {..} (ActionSpec action, outSpecs) =
    runRealModeDo (hoistNodeResources unwrap nr) outSpecs $
    ActionSpec $ \vI sendActions ->
        unwrap . action vI $ hoistSendActions wrap unwrap sendActions

-- | RealMode runner.
runRealModeDo
    :: forall ssc a.
       (SscConstraint ssc, SecurityWorkersClass ssc)
    => NodeResources ssc (RealMode ssc)
    -> OutSpecs
    -> ActionSpec (RealMode ssc) a
    -> Production a
runRealModeDo NodeResources {..} outSpecs action =
    specialDiscoveryWrapper $ do
        jsonLogConfig <- maybe
            (pure JsonLogDisabled)
            jsonLogConfigFromHandle
            nrJLogHandle

        oq <- initQueue ncNetworkConfig

        runToProd jsonLogConfig $
          runServer ncNetworkConfig
                    (simpleNodeEndPoint nrTransport)
                    (const noReceiveDelay)
                    allListeners
                    outSpecs
                    startMonitoring
                    stopMonitoring
                    oq
                    action
  where
    NodeContext {..} = nrContext
    NodeParams {..} = ncNodeParams
    LoggingParams {..} = bpLoggingParams npBaseParams
    startMonitoring node' =
        case npEnableMetrics of
            False -> return Nothing
            True  -> Just <$> do
                ekgStore' <- setupMonitor
                    (runProduction . runToProd JsonLogDisabled) node' nrEkgStore
                liftIO $ Metrics.registerGcMetrics ekgStore'
                mEkgServer <- case npEkgParams of
                    Nothing -> return Nothing
                    Just (EkgParams {..}) -> Just <$> do
                        liftIO $ Monitoring.forkServerWith ekgStore' ekgHost ekgPort
                mStatsdServer <- case npStatsdParams of
                    Nothing -> return Nothing
                    Just (StatsdParams {..}) -> Just <$> do
                        let statsdOptions = Monitoring.defaultStatsdOptions
                                { Monitoring.host = statsdHost
                                , Monitoring.port = statsdPort
                                , Monitoring.flushInterval = statsdInterval
                                , Monitoring.debug = statsdDebug
                                , Monitoring.prefix = statsdPrefix
                                , Monitoring.suffix = statsdSuffix
                                }
                        liftIO $ Monitoring.forkStatsd statsdOptions ekgStore'
                return (mEkgServer, mStatsdServer)

    stopMonitoring Nothing = return ()
    stopMonitoring (Just (mEkg, mStatsd)) = do
        whenJust mStatsd (killThread . Monitoring.statsdThreadId)
        whenJust mEkg stopMonitor

    -- TODO: it would be good to put this behavior into 'Discovery' class.
    -- TODO: there's really no reason why we have to continually rejoin the
    -- network. In fact it runs contrary to the intended use of Kademlia.
    -- Joining a network which you've already joined *should* give an ID
    -- clash with high probability (the initial peer probably remembers you),
    -- but we've actually modified the Kademlia library to ignore this
    -- just because we abuse it here in cardano-sl.
    specialDiscoveryWrapper = case ncDiscoveryContext of
        DCStatic _          -> identity
        DCKademlia kademlia -> foreverRejoinNetwork kademlia

    runToProd :: forall t .
                 JsonLogConfig
              -> RealMode ssc t
              -> Production t
    runToProd jlConf act = Mtl.runReaderT act $
        RealModeContext
            nrDBs
            nrSscState
            nrTxpState
            nrDlgState
            jlConf
            lpRunnerTag
            nrContext

newtype EnqueuedConversation m t =
    EnqueuedConversation (Msg, NodeId -> PeerData -> N.Conversation PackingType m t)

instance FormatMsg (EnqueuedConversation m) where
    formatMsg = (\k (EnqueuedConversation (msg, _)) -> k msg) <$> shown

type OQ m = OQ.OutboundQ (EnqueuedConversation m) NodeId

sendMsgFromConverse
    :: N.Converse PackingType PeerData m
    -> OQ.SendMsg m (EnqueuedConversation m) NodeId
sendMsgFromConverse converse (EnqueuedConversation (_, k)) nodeId =
    N.converseWith converse nodeId (k nodeId)

oqEnqueue
    :: ( Mockable Throw m, MonadIO m, WithLogger m )
    => OQ m
    -> Msg
    -> (NodeId -> VerInfo -> N.Conversation PackingType m t)
    -> m (Map NodeId (m t))
oqEnqueue oq msgType k = do
    itList <- OQ.enqueue oq msgType (EnqueuedConversation (msgType, k))
    let itMap = M.fromList itList
    return ((>>= either throw return) <$> itMap)

oqDequeue
    :: ( MonadIO m
       , MonadMockable m
       , WithLogger m
       )
    => OQ m
    -> N.Converse PackingType PeerData m
    -> m (m ())
oqDequeue oq converse = do
    it <- async $ OQ.dequeueThread oq (sendMsgFromConverse converse)
    return (cancel it)

initQueue
    :: (MonadIO m, MonadIO m', MonadMockable m', WithLogger m')
    => NetworkConfig
    -> m (OQ m')
initQueue NetworkConfig{..} =
    -- TODO: Find better self identifier (for improved logging)
    OQ.new ("self" :: String) enqueuePolicy dequeuePolicy failurePolicy classification
  where
    ourNodeType = ncNodeType
    enqueuePolicy = OQ.defaultEnqueuePolicy ourNodeType
    dequeuePolicy = OQ.defaultDequeuePolicy ourNodeType
    failurePolicy = OQ.defaultFailurePolicy ourNodeType
    -- TODO use NetworkConfig to make a static classification
    -- TODO make that classification dynamic?
    classification = mempty

runServer
    :: forall m t b .
       (MonadIO m, MonadMockable m, MonadFix m, WithLogger m, WithPeerState m)
    => NetworkConfig
    -> (m (Statistics m) -> NodeEndPoint m)
    -> (m (Statistics m) -> ReceiveDelay m)
    -> (EnqueueMsg m -> MkListeners m)
    -> OutSpecs
    -> (Node m -> m t)
    -> (t -> m ())
    -> OQ m
    -> ActionSpec m b
    -> m b
runServer NetworkConfig {..} mkTransport mkReceiveDelay mkL (OutSpecs wouts) withNode afterNode oq (ActionSpec action) = do
    let enq :: EnqueueMsg m
        enq = makeEnqueueMsg ourVerInfo (oqEnqueue oq)
        mkL' = mkL enq
        InSpecs ins = inSpecs mkL'
        OutSpecs outs = outSpecs mkL'
        ourVerInfo =
            VerInfo Const.protocolMagic Const.lastKnownBlockVersion ins $ outs <> wouts
        mkListeners' theirVerInfo =
            mkListeners mkL' ourVerInfo theirVerInfo
    stdGen <- liftIO newStdGen
    logInfo $ sformat ("Our verInfo: "%build) ourVerInfo
    node mkTransport mkReceiveDelay mkConnectDelay stdGen BiP ourVerInfo defaultNodeEnvironment $ \__node ->
        NodeAction mkListeners' $ \converse ->
            let sendActions :: SendActions m
                sendActions = makeSendActions ourVerInfo (oqEnqueue oq) converse
            in  bracket (acquire converse __node) release (const (action ourVerInfo sendActions))
  where
    acquire converse __node = do
        stopDequeue <- oqDequeue oq converse
        other <- withNode __node
        return (stopDequeue, other)
    release (stopDequeue, other) = do
        stopDequeue
        afterNode other
    mkConnectDelay = const (pure Nothing)

