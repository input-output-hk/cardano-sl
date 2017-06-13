{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS -fno-cross-module-specialise #-}

-- | Runners in various modes.

module Pos.Launcher.Runner
       ( -- * High level runners
         runRealMode
       , runRealBasedMode

       -- * Exported for custom usage in CLI utils
       , runServer
       ) where

import           Universum                   hiding (finally)

import           Control.Monad.Fix           (MonadFix)
import           Data.Tagged                 (Tagged (..))
import qualified Ether
import           Formatting                  (build, sformat, (%))
import           Mockable                    (MonadMockable, Production (..), finally)
import           Network.Transport.Abstract  (Transport)
import           Node                        (Node, NodeAction (..),
                                              defaultNodeEnvironment, hoistSendActions,
                                              node, simpleNodeEndPoint)
import           Node.Util.Monitor           (setupMonitor, stopMonitor)
import qualified System.Metrics              as Metrics
import           System.Random               (newStdGen)
import qualified System.Remote.Monitoring    as Monitoring
import           System.Wlog                 (WithLogger, logDebug, logInfo,
                                              usingLoggerName)

import           Pos.Binary                  ()
import           Pos.Block.BListener         (runBListenerStub)
import           Pos.Communication           (ActionSpec (..), BiP (..), InSpecs (..),
                                              MkListeners (..), OutSpecs (..),
                                              VerInfo (..), allListeners,
                                              hoistMkListeners)
import           Pos.Communication.PeerState (PeerStateTag, runPeerStateRedirect)
import qualified Pos.Constants               as Const
import           Pos.Context                 (NodeContext (..))
import           Pos.DB                      (NodeDBs, runDBPureRedirect)
import           Pos.DB.Block                (runBlockDBRedirect)
import           Pos.DB.DB                   (runGStateCoreRedirect)
import           Pos.Delegation.Class        (DelegationVar)
import           Pos.DHT.Real                (foreverRejoinNetwork)
import           Pos.Discovery               (DiscoveryContextSum (..),
                                              runDiscoveryRedirect)
import           Pos.Launcher.Param          (BaseParams (..), LoggingParams (..),
                                              NodeParams (..))
import           Pos.Launcher.Resource       (NodeResources (..), hoistNodeResources)
import           Pos.Security                (SecurityWorkersClass)
import           Pos.Slotting                (runSlotsDataRedirect, runSlotsRedirect)
import           Pos.Ssc.Class               (SscConstraint)
import           Pos.Ssc.Extra               (SscMemTag)
import           Pos.Txp.MemState            (TxpHolderTag)
import           Pos.Util.TimeWarp           (runJsonLogT')
import           Pos.WorkMode                (RealMode (..), WorkMode)

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
                        ekgStore' <- setupMonitor (runProduction . runToProd Nothing)
                            node' ekgStore
                        liftIO $ Metrics.registerGcMetrics ekgStore'
                        liftIO $ Monitoring.forkServerWith ekgStore' "127.0.0.1" port

        let stopMonitoring it = whenJust it stopMonitor

        runToProd nrJLogHandle $
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

    runToProd :: forall t . Maybe Handle -> RealMode ssc t -> Production t
    runToProd jlHandle (RealMode act) =
        usingLoggerName lpRunnerTag .
            runJsonLogT' jlHandle .
            flip Ether.runReadersT nrContext .
            flip Ether.runReadersT
                ( Tagged @NodeDBs nrDBs
                , Tagged @SscMemTag nrSscState
                , Tagged @TxpHolderTag nrTxpState
                , Tagged @DelegationVar nrDlgState
                , Tagged @PeerStateTag nrPeerState
                ) .
            runDBPureRedirect .
            runBlockDBRedirect .
            runSlotsDataRedirect .
            runSlotsRedirect .
            runDiscoveryRedirect .
            runPeerStateRedirect .
            runGStateCoreRedirect .
            runBListenerStub $
            act
{-# NOINLINE runRealMode #-}

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
