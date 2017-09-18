{-# LANGUAGE CPP        #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

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
import           Formatting                      (build, sformat, (%))
import           Mockable                        (Mockable, MonadMockable,
                                                  Production (..), Throw, async, bracket,
                                                  cancel, killThread, throw)
import qualified Network.Broadcast.OutboundQueue as OQ
import           Node                            (Node, NodeAction (..), NodeEndPoint,
                                                  ReceiveDelay, Statistics,
                                                  defaultNodeEnvironment, noReceiveDelay,
                                                  node, simpleNodeEndPoint)
import qualified Node.Conversation               as N (Conversation, Converse,
                                                       converseWith)
import           Node.Util.Monitor               (registerMetrics)
import           Pos.System.Metrics.Constants    (cardanoNamespace)
import           Pos.Util.Monitor                (stopMonitor)
import qualified System.Metrics                  as Metrics
import           System.Random                   (newStdGen)
import qualified System.Remote.Monitoring.Statsd as Monitoring
import qualified System.Remote.Monitoring.Wai    as Monitoring
import           System.Wlog                     (WithLogger, logInfo)

import           Pos.Binary                      ()
import           Pos.Communication               (ActionSpec (..), EnqueueMsg,
                                                  InSpecs (..), MkListeners (..), Msg,
                                                  OutSpecs (..), PackingType, PeerData,
                                                  SendActions, VerInfo (..), allListeners,
                                                  bipPacking, hoistSendActions,
                                                  makeEnqueueMsg, makeSendActions)
import           Pos.Configuration               (HasNodeConfiguration)
import           Pos.Context                     (NodeContext (..))
import           Pos.Core.Types                  (ProtocolMagic (..))
import           Pos.Core.Configuration          (HasConfiguration, protocolMagic)
import           Pos.Infra.Configuration         (HasInfraConfiguration)
import           Pos.Launcher.Param              (BaseParams (..), LoggingParams (..),
                                                  NodeParams (..))
import           Pos.Launcher.Resource           (NodeResources (..), hoistNodeResources)
import           Pos.Network.Types               (NetworkConfig (..), NodeId, initQueue)
import           Pos.Ssc.Class                   (SscConstraint)
import           Pos.Statistics                  (EkgParams (..), StatsdParams (..))
import           Pos.Update.Configuration        (HasUpdateConfiguration, lastKnownBlockVersion)
import           Pos.Util.JsonLog                (JsonLogConfig (..),
                                                  jsonLogConfigFromHandle)
import           Pos.WorkMode                    (EnqueuedConversation (..), OQ, RealMode,
                                                  RealModeContext (..), WorkMode)

----------------------------------------------------------------------------
-- High level runners
----------------------------------------------------------------------------

-- | Run activity in 'RealMode'.
runRealMode
    :: forall ssc ctx a.
       (SscConstraint ssc, WorkMode ssc ctx (RealMode ssc))
    => NodeResources ssc (RealMode ssc)
    -> (ActionSpec (RealMode ssc) a, OutSpecs)
    -> Production a
runRealMode = runRealBasedMode identity identity

-- | Run activity in something convertible to 'RealMode' and back.
runRealBasedMode
    :: forall ssc ctx m a.
       (SscConstraint ssc, WorkMode ssc ctx m)
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
       ( HasConfiguration
       , HasInfraConfiguration
       , HasUpdateConfiguration
       , HasNodeConfiguration
       , SscConstraint ssc
       )
    => NodeResources ssc (RealMode ssc)
    -> OutSpecs
    -> ActionSpec (RealMode ssc) a
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
                    (startMonitoring oq)
                    stopMonitoring
                    oq
                    action
  where
    NodeContext {..} = nrContext
    NetworkConfig {..} = ncNetworkConfig
    NodeParams {..} = ncNodeParams
    LoggingParams {..} = bpLoggingParams npBaseParams
    startMonitoring oq node' =
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
                return (mEkgServer, mStatsdServer)

    stopMonitoring Nothing = return ()
    stopMonitoring (Just (mEkg, mStatsd)) = do
        whenJust mStatsd (killThread . Monitoring.statsdThreadId)
        whenJust mEkg stopMonitor

    runToProd :: forall t .
                 JsonLogConfig
              -> OQ (RealMode ssc)
              -> RealMode ssc t
              -> Production t
    runToProd jlConf oq act = Mtl.runReaderT act $
        RealModeContext
            nrDBs
            nrSscState
            nrTxpState
            nrDlgState
            jlConf
            lpRunnerTag
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
    stdGen <- liftIO newStdGen
    logInfo $ sformat ("Our verInfo: "%build) ourVerInfo
    node mkTransport mkReceiveDelay mkConnectDelay stdGen bipPacking ourVerInfo defaultNodeEnvironment $ \__node ->
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
