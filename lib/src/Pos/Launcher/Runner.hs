{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RankNTypes          #-}

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

import           Universum hiding (bracket)

import           Control.Monad.Fix (MonadFix)
import qualified Control.Monad.Reader as Mtl
import           Data.Default (Default)
import qualified Data.Map as M
import           Formatting (build, sformat, (%))
import           Mockable (Mockable, MonadMockable, Production (..), Throw, async, bracket, cancel,
                           killThread, throw)
import qualified Network.Broadcast.OutboundQueue as OQ
import           Node (Node, NodeAction (..), NodeEndPoint, ReceiveDelay, Statistics,
                       defaultNodeEnvironment, noReceiveDelay, node, nodeAckTimeout,
                       simpleNodeEndPoint)
import qualified Node.Conversation as N (Conversation, Converse, converseWith)
import           Node.Util.Monitor (registerMetrics)
import           Pos.System.Metrics.Constants (cardanoNamespace)
import           Pos.Util.Monitor (stopMonitor)
import qualified System.Metrics as Metrics
import           System.Random (newStdGen)
import qualified System.Remote.Monitoring.Statsd as Monitoring
import qualified System.Remote.Monitoring.Wai as Monitoring
import           System.Wlog (WithLogger, logInfo)

import           Pos.Binary ()
import           Pos.Communication (ActionSpec (..), EnqueueMsg, InSpecs (..), MkListeners (..),
                                    Msg, OutSpecs (..), PackingType, PeerData, SendActions,
                                    VerInfo (..), allListeners, bipPacking, hoistSendActions,
                                    makeEnqueueMsg, makeSendActions)
import           Pos.Configuration (HasNodeConfiguration, conversationEstablishTimeout)
import           Pos.Context (NodeContext (..))
import           Pos.Core.Configuration (HasConfiguration, protocolMagic)
import           Pos.Crypto.Configuration (ProtocolMagic (..))
import           Pos.Launcher.Configuration (HasConfigurations)
import           Pos.Launcher.Param (BaseParams (..), LoggingParams (..), NodeParams (..))
import           Pos.Launcher.Resource (NodeResources (..), hoistNodeResources)
import           Pos.Network.Types (NetworkConfig (..), NodeId, initQueue,
                                    topologyRoute53HealthCheckEnabled)
import           Pos.Recovery.Instance ()
import           Pos.Statistics (EkgParams (..), StatsdParams (..))
import           Pos.Txp (MonadTxpLocal)
import           Pos.Update.Configuration (HasUpdateConfiguration, lastKnownBlockVersion)
import           Pos.Util.CompileInfo (HasCompileInfo)
import           Pos.Util.JsonLog (JsonLogConfig (..), jsonLogConfigFromHandle)
import           Pos.Web.Server (route53HealthCheckApplication, serveImpl)
import           Pos.WorkMode (EnqueuedConversation (..), OQ, RealMode, RealModeContext (..),
                               WorkMode)

----------------------------------------------------------------------------
-- High level runners
----------------------------------------------------------------------------

-- | Run activity in 'RealMode'.
runRealMode
    :: forall ext ctx a.
       (HasCompileInfo, WorkMode ctx (RealMode ext))
    => NodeResources ext (RealMode ext)
    -> (ActionSpec (RealMode ext) a, OutSpecs)
    -> Production a
runRealMode = runRealBasedMode @ext @ctx identity identity

-- | Run activity in something convertible to 'RealMode' and back.
runRealBasedMode
    :: forall ext ctx m a.
       ( HasCompileInfo
       , WorkMode ctx m
       , Default ext
       , MonadTxpLocal (RealMode ext)
       -- MonadTxpLocal is meh,
       -- we can't remove @ext@ from @RealMode@ because
       -- explorer and wallet use RealMode,
       -- though they should use only @RealModeContext@
       )
    => (forall b. m b -> RealMode ext b)
    -> (forall b. RealMode ext b -> m b)
    -> NodeResources ext m
    -> (ActionSpec m a, OutSpecs)
    -> Production a
runRealBasedMode unwrap wrap nr@NodeResources {..} (ActionSpec action, outSpecs) =
    runRealModeDo (hoistNodeResources unwrap nr) outSpecs $
    ActionSpec $ \vI sendActions ->
        unwrap . action vI $ hoistSendActions wrap unwrap sendActions

-- | RealMode runner.
runRealModeDo
    :: forall ext a.
       ( HasConfigurations
       , HasCompileInfo
       , Default ext
       , MonadTxpLocal (RealMode ext)
       )
    => NodeResources ext (RealMode ext)
    -> OutSpecs
    -> ActionSpec (RealMode ext) a
    -> Production a
runRealModeDo NodeResources {..} outSpecs action =
    do
        jsonLogConfig <- maybe
            (pure JsonLogDisabled)
            jsonLogConfigFromHandle
            nrJLogHandle

        oq <- initQueue ncNetworkConfig (Just nrEkgStore)

        runToProd jsonLogConfig oq $
          runServer (simpleNodeEndPoint nrTransport)
                    (const noReceiveDelay)
                    (allListeners oq ncTopology)
                    outSpecs
                    (startMonitoring ncTopology oq)
                    stopMonitoring
                    oq
                    action
  where
    NodeContext {..} = nrContext
    NetworkConfig {..} = ncNetworkConfig
    NodeParams {..} = ncNodeParams
    LoggingParams {..} = bpLoggingParams npBaseParams
    -- Expose the health-check endpoint for DNS load-balancing
    -- and optionally other services as EKG & statsd.
    startMonitoring topology oq node' = do
        -- Expose the health-check
        let (hcHost, hcPort) = case npRoute53Params of
                Nothing         -> ("127.0.0.1", 3030)
                Just (hst, prt) -> (decodeUtf8 hst, fromIntegral prt)
        mRoute53HealthCheck <- case topologyRoute53HealthCheckEnabled topology of
            False -> return Nothing
            True  -> let app = route53HealthCheckApplication topology oq
                     in Just <$> async (serveImpl app hcHost hcPort Nothing)
        -- Run the optional tools.
        case npEnableMetrics of
            False -> return Nothing
            True  -> Just <$> do
                registerMetrics (Just cardanoNamespace) (runProduction . runToProd JsonLogDisabled oq) node' nrEkgStore
                liftIO $ Metrics.registerGcMetrics nrEkgStore
                mEkgServer <- case npEkgParams of
                    Nothing -> return Nothing
                    Just (EkgParams {..}) -> Just <$> do
                        liftIO $ Monitoring.forkServerWith nrEkgStore ekgHost ekgPort
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
                        liftIO $ Monitoring.forkStatsd statsdOptions nrEkgStore
                return (mEkgServer, mStatsdServer, mRoute53HealthCheck)

    stopMonitoring Nothing = return ()
    stopMonitoring (Just (mEkg, mStatsd, mRoute53HealthCheck)) = do
        whenJust mStatsd (killThread . Monitoring.statsdThreadId)
        whenJust mEkg stopMonitor
        whenJust mRoute53HealthCheck cancel

    runToProd :: forall t .
                 JsonLogConfig
              -> OQ (RealMode ext)
              -> RealMode ext t
              -> Production t
    runToProd jlConf oq act = Mtl.runReaderT act $
        RealModeContext
            nrDBs
            nrSscState
            nrTxpState
            nrDlgState
            jlConf
            lpDefaultName
            nrContext
            oq

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

runServer
    :: forall m t b .
       ( MonadIO m
       , MonadMockable m
       , MonadFix m
       , WithLogger m
       , HasConfiguration
       , HasUpdateConfiguration
       , HasNodeConfiguration
       )
    => (m (Statistics m) -> NodeEndPoint m)
    -> (m (Statistics m) -> ReceiveDelay m)
    -> (EnqueueMsg m -> MkListeners m)
    -> OutSpecs
    -> (Node m -> m t)
    -> (t -> m ())
    -> OQ m
    -> ActionSpec m b
    -> m b
runServer mkTransport mkReceiveDelay mkL (OutSpecs wouts) withNode afterNode oq (ActionSpec action) = do
    let enq :: EnqueueMsg m
        enq = makeEnqueueMsg ourVerInfo (oqEnqueue oq)
        mkL' = mkL enq
        InSpecs ins = inSpecs mkL'
        OutSpecs outs = outSpecs mkL'
        ourVerInfo =
            VerInfo (getProtocolMagic protocolMagic) lastKnownBlockVersion ins $ outs <> wouts
        mkListeners' theirVerInfo =
            mkListeners mkL' ourVerInfo theirVerInfo
        nodeEnv = defaultNodeEnvironment { nodeAckTimeout = conversationEstablishTimeout }
    stdGen <- liftIO newStdGen
    logInfo $ sformat ("Our verInfo: "%build) ourVerInfo
    node mkTransport mkReceiveDelay mkConnectDelay stdGen bipPacking ourVerInfo nodeEnv $ \__node ->
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
