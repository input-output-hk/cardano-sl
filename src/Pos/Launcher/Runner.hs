{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Runners in various modes.

module Pos.Launcher.Runner
       ( -- * High level runners
         runRealMode
       , runRealBasedMode

       -- * Exported for custom usage in CLI utils
       , runServer
       ) where

import           Universum                  hiding (finally)

import           Control.Monad.Fix          (MonadFix)
import qualified Control.Monad.Reader       as Mtl
import           Formatting                 (build, sformat, (%))
import           Mockable                   (MonadMockable, Production (..), finally)
import           Network.Transport.Abstract (Transport)
import           Node                       (Node, NodeAction (..),
                                             defaultNodeEnvironment, hoistSendActions,
                                             node, simpleNodeEndPoint)
import           Node.Util.Monitor          (setupMonitor, stopMonitor)
import qualified System.Metrics             as Metrics
import           System.Random              (newStdGen)
import qualified System.Remote.Monitoring   as Monitoring
import           System.Wlog                (WithLogger, logDebug, logInfo)

import           Pos.Binary                 ()
import           Pos.Communication          (ActionSpec (..), BiP (..), InSpecs (..),
                                             MkListeners (..), OutSpecs (..),
                                             VerInfo (..), allListeners, hoistMkListeners)
import qualified Pos.Constants              as Const
import           Pos.Context                (NodeContext (..))
import           Pos.DHT.Real               (foreverRejoinNetwork)
import           Pos.Discovery              (DiscoveryContextSum (..))
import           Pos.Launcher.Param         (BaseParams (..), LoggingParams (..),
                                             NodeParams (..))
import           Pos.Launcher.Resource      (NodeResources (..), hoistNodeResources)
import           Pos.Security               (SecurityWorkersClass)
import           Pos.Ssc.Class              (SscConstraint)
import           Pos.Util.JsonLog           (JsonLogConfig (..), jsonLogConfigFromHandle)
import           Pos.WorkMode               (RealMode, RealModeContext (..), WorkMode,
                                             unRealMode)

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
    :: forall ssc m a.
       (SscConstraint ssc, SecurityWorkersClass ssc, WorkMode ssc m)
    => (forall b. m b -> RealMode ssc b)
    -> (forall b. RealMode ssc b -> m b)
    -> NodeResources ssc m
    -> (ActionSpec m a, OutSpecs)
    -> Production a
runRealBasedMode unwrap wrap nr@NodeResources {..} (ActionSpec action, outSpecs) =
    runRealModeDo (hoistNodeResources unwrap nr) listeners outSpecs $
    ActionSpec $ \vI sendActions ->
        unwrap . action vI $ hoistSendActions wrap unwrap sendActions
  where
    listeners = hoistMkListeners unwrap wrap allListeners

-- | RealMode runner.
runRealModeDo
    :: forall ssc a.
       (SscConstraint ssc, SecurityWorkersClass ssc)
    => NodeResources ssc (RealMode ssc)
    -> MkListeners (RealMode ssc)
    -> OutSpecs
    -> ActionSpec (RealMode ssc) a
    -> Production a
runRealModeDo NodeResources {..} listeners outSpecs action =
    specialDiscoveryWrapper $ do
        ekgStore <- liftIO $ Metrics.newStore
        -- To start monitoring, add the time-warp metrics and the GC
        -- metrics then spin up the server.
        let startMonitoring node' = case lpEkgPort of
                Nothing   -> return Nothing
                Just port -> Just <$> do
                        ekgStore' <- setupMonitor (runProduction . runToProd JsonLogDisabled)
                            node' ekgStore
                        liftIO $ Metrics.registerGcMetrics ekgStore'
                        liftIO $ Monitoring.forkServerWith ekgStore' "127.0.0.1" port

        let stopMonitoring it = whenJust it stopMonitor
        jsonLogConfig <- maybe
            (pure JsonLogDisabled)
            jsonLogConfigFromHandle
            nrJLogHandle

        runToProd jsonLogConfig $
            runServer nrTransport listeners outSpecs
            startMonitoring stopMonitoring action
  where
    NodeContext {..} = nrContext
    NodeParams {..} = ncNodeParams
    LoggingParams {..} = bpLoggingParams npBaseParams

    -- TODO: it would be good to put this behavior into 'Discovery' class.
    specialDiscoveryWrapper = case ncDiscoveryContext of
        DCStatic _          -> identity
        DCKademlia kademlia -> foreverRejoinNetwork kademlia

    runToProd :: forall t . JsonLogConfig -> RealMode ssc t -> Production t
    runToProd jlConf act = Mtl.runReaderT (unRealMode act) $
        RealModeContext
            nrDBs
            nrSscState
            nrTxpState
            nrDlgState
            nrPeerState
            jlConf
            lpRunnerTag
            nrContext

runServer
    :: (MonadIO m, MonadMockable m, MonadFix m, WithLogger m)
    => Transport m
    -> MkListeners m
    -> OutSpecs
    -> (Node m -> m t)
    -> (t -> m ())
    -> ActionSpec m b
    -> m b
runServer transport mkL (OutSpecs wouts) withNode afterNode (ActionSpec action) = do
    stdGen <- liftIO newStdGen
    logInfo $ sformat ("Our verInfo: "%build) ourVerInfo
    node (simpleNodeEndPoint transport) (const $ pure Nothing) stdGen BiP ourVerInfo defaultNodeEnvironment $ \__node ->
        NodeAction mkListeners' $ \sendActions -> do
            t <- withNode __node
            action ourVerInfo sendActions `finally` afterNode t
  where
    InSpecs ins = inSpecs mkL
    OutSpecs outs = outSpecs mkL
    ourVerInfo =
        VerInfo Const.protocolMagic Const.lastKnownBlockVersion ins $ outs <> wouts
    mkListeners' theirVerInfo = do
        logDebug $ sformat ("Incoming connection: theirVerInfo="%build) theirVerInfo
        mkListeners mkL ourVerInfo theirVerInfo
