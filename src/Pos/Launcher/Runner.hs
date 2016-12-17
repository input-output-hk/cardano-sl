{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | Runners in various modes.

module Pos.Launcher.Runner
       (
         -- * High level runners
         runRawRealMode
       , runProductionMode
       , runStatsMode
       , runServiceMode

         -- * Service runners
       , runSupporterReal
       , runTimeSlaveReal
       , runTimeLordReal

       -- * Exported for custom usage in CLI utils
       , addDevListeners
       , setupLoggers
       , bracketDHTInstance
       , runTimed
       , runKDHT
       ) where

import           Control.Concurrent.MVar         (newEmptyMVar, newMVar, takeMVar,
                                                  tryReadMVar)
import           Control.Monad                   (fail)
import           Control.Monad.Catch             (bracket)
import           Control.Monad.Trans.Control     (MonadBaseControl)
import           Control.Monad.Trans.Resource    (allocate, runResourceT)
import           Control.TimeWarp.Rpc            (Dialog, Transfer, commLoggerName,
                                                  runDialog, runTransfer)
import           Control.TimeWarp.Timed          (MonadTimed, currentTime, fork,
                                                  killThread, repeatForever, runTimedIO,
                                                  sec)

import           Data.Acquire                    (withEx)
import           Data.List                       (nub)
import qualified Data.Time                       as Time
import           Formatting                      (build, sformat, shown, (%))
import           System.Directory                (doesDirectoryExist,
                                                  removeDirectoryRecursive)
import           System.FilePath                 ((</>))
import           System.Wlog                     (LoggerName (..), WithLogger, logDebug,
                                                  logInfo, logWarning, releaseAllHandlers,
                                                  traverseLoggerConfig, usingLoggerName)
import           Universum

import           Pos.Binary                      ()
import           Pos.CLI                         (readLoggerConfig)
import           Pos.Communication               (SysStartRequest (..), allListeners,
                                                  noCacheMessageNames,
                                                  sysStartReqListener,
                                                  sysStartReqListenerSlave,
                                                  sysStartRespListener)
import           Pos.Constants                   (RunningMode (..), defaultPeers,
                                                  isDevelopment, runningMode)
import           Pos.DHT.Model                   (BiP (..), ListenerDHT, MonadDHT (..),
                                                  mapListenerDHT, sendToNeighbors)
#ifdef WITH_ROCKS
import qualified Pos.Modern.DB                   as Modern
import qualified Pos.Modern.Txp.Holder           as Modern
import qualified Pos.Modern.Txp.Storage.UtxoView as Modern
#endif
import           Pos.Context                     (ContextHolder (..), NodeContext (..),
                                                  runContextHolder)
import           Pos.DHT.Model.Class             (DHTPacking, MonadDHTDialog)
import           Pos.DHT.Real                    (KademliaDHT, KademliaDHTConfig (..),
                                                  KademliaDHTInstance,
                                                  KademliaDHTInstanceConfig (..),
                                                  runKademliaDHT, startDHTInstance,
                                                  stopDHTInstance)
import           Pos.Launcher.Param              (BaseParams (..), LoggingParams (..),
                                                  NodeParams (..))
import           Pos.Ssc.Class                   (SscConstraint, SscNodeContext,
                                                  SscParams, sscCreateNodeContext)
import           Pos.Ssc.LocalData               (runSscLDImpl)
import           Pos.State                       (NodeState, closeState, openMemState,
                                                  openState, runDBHolder)
import           Pos.State.Storage               (storageFromUtxo)
import           Pos.Statistics                  (getNoStatsT, runStatsT)
import           Pos.Types                       (Timestamp (Timestamp), timestampF)
import           Pos.Util                        (runWithRandomIntervals)
import           Pos.Worker                      (statsWorkers)
import           Pos.WorkMode                    (MinWorkMode, ProductionMode,
                                                  RawRealMode, ServiceMode, SocketState,
                                                  StatsMode, runTxLDImpl)

----------------------------------------------------------------------------
-- Service node runners
----------------------------------------------------------------------------

-- | Runs node as time-slave inside IO monad.
runTimeSlaveReal :: KademliaDHTInstance -> BaseParams -> IO Timestamp
runTimeSlaveReal inst bp = do
    mvar <- liftIO newEmptyMVar
    runServiceMode inst bp (listeners mvar) $
      case runningMode of
         Development -> do
           tId <- fork $
             runWithRandomIntervals (sec 10) (sec 60) $ liftIO (tryReadMVar mvar) >>= \case
                 Nothing -> do
                    logInfo "Asking neighbors for system start"
                    (void $ sendToNeighbors SysStartRequest) `catchAll`
                       \e -> logDebug $ sformat
                       ("Error sending SysStartRequest to neighbors: " % shown) e
                 Just _ -> fail "Close thread"
           t <- liftIO $ takeMVar mvar
           killThread tId
           t <$ logInfo (sformat ("[Time slave] adopted system start " % timestampF) t)
         Production ts -> logWarning "Time slave launched in Production" $> ts
  where
    listeners mvar =
      if isDevelopment
         then [sysStartReqListenerSlave, sysStartRespListener mvar]
         else []

-- | Runs time-lord to acquire system start.
runTimeLordReal :: LoggingParams -> IO Timestamp
runTimeLordReal lp@LoggingParams{..} = loggerBracket lp $ do
    t <- getCurTimestamp
    usingLoggerName lpRunnerTag (doLog t) $> t
  where
    doLog t = do
        realTime <- liftIO Time.getZonedTime
        logInfo (sformat ("[Time lord] System start: " %timestampF%", i. e.: "%shown) t realTime)

runSupporterReal :: KademliaDHTInstance -> BaseParams -> IO ()
runSupporterReal inst bp = runServiceMode inst bp [] $ do
    supporterKey <- currentNodeKey
    logInfo $ sformat ("Supporter key: " % build) supporterKey
    repeatForever (sec 5) (const . return $ sec 5) $
        getKnownPeers >>= logInfo . sformat ("Known peers: " % build)

----------------------------------------------------------------------------
-- High level runners
----------------------------------------------------------------------------

-- | RawRealMode runner.
runRawRealMode
    :: forall ssc c.
       SscConstraint ssc
    => KademliaDHTInstance
    -> NodeParams
    -> SscParams ssc
    -> [ListenerDHT SocketState (RawRealMode ssc)]
    -> RawRealMode ssc c
    -> IO c
runRawRealMode inst np@NodeParams {..} sscnp listeners action = runResourceT $ do
    lift $ setupLoggers lp
    legacyDB <- snd <$> allocate openDb closeDb
#ifdef WITH_ROCKS
    modernDBs <- Modern.openNodeDBs (npDbPathM </> "zhogovo")
    let initTip = notImplemented -- init tip must be here
#endif
    let run db =
            runTimed lpRunnerTag .
            runDBHolder db .
#ifdef WITH_ROCKS
            Modern.runDBHolder modernDBs .
#endif
            withEx (sscCreateNodeContext @ssc sscnp) $ flip (runCH np) .
            runSscLDImpl .
            runTxLDImpl .
#ifdef WITH_ROCKS
            flip Modern.runTxpLDHolderUV (Modern.createFromDB . Modern._utxoDB $ modernDBs) .
#endif
            runKDHT inst npBaseParams listeners $
            nodeStartMsg npBaseParams >> action
    lift $ run legacyDB
  where
    lp@LoggingParams {..} = bpLoggingParams npBaseParams
    mStorage = storageFromUtxo <$> npCustomUtxo
    openDb :: IO (NodeState ssc)
    openDb = do
        -- we rebuild DB manually, because we need to remove
        -- everything in npDbPath
        let rebuild fp =
                whenM ((npRebuildDb &&) <$> doesDirectoryExist fp) $
                removeDirectoryRecursive fp
        whenJust npDbPath rebuild
        runTimed lpRunnerTag $
            maybe
                (openMemState mStorage)
                (openState mStorage False)
                ((</> "main") <$> npDbPath)
    closeDb :: NodeState ssc -> IO ()
    closeDb = closeState

-- | ProductionMode runner.
runProductionMode
    :: forall ssc a.
       SscConstraint ssc
    => KademliaDHTInstance -> NodeParams -> SscParams ssc ->
       ProductionMode ssc a -> IO a
runProductionMode inst np@NodeParams {..} sscnp =
    runRawRealMode inst np sscnp listeners . getNoStatsT
  where
    listeners = addDevListeners npSystemStart noStatsListeners
    noStatsListeners = map (mapListenerDHT getNoStatsT) (allListeners @ssc)

-- | StatsMode runner.
-- [CSL-169]: spawn here additional listener, which would accept stat queries
-- can be done as part of refactoring (or someone who will refactor will create new issue).
runStatsMode
    :: forall ssc a.
       SscConstraint ssc
    => KademliaDHTInstance -> NodeParams -> SscParams ssc -> StatsMode ssc a
    -> IO a
runStatsMode inst np@NodeParams {..} sscnp action =
    runRawRealMode inst np sscnp listeners $ runStatsT $ do
    mapM_ fork statsWorkers
    action
  where
    listeners = addDevListeners npSystemStart sListeners
    sListeners = map (mapListenerDHT runStatsT) $ allListeners @ssc

-- | ServiceMode runner.
runServiceMode
    :: KademliaDHTInstance
    -> BaseParams
    -> [ListenerDHT SocketState ServiceMode]
    -> ServiceMode a
    -> IO a
runServiceMode inst bp@BaseParams{..} listeners action = loggerBracket bpLoggingParams $ do
    runTimed (lpRunnerTag bpLoggingParams) . runKDHT inst bp listeners $
        nodeStartMsg bp >> action

----------------------------------------------------------------------------
-- Lower level runners
----------------------------------------------------------------------------

runKDHT
    :: ( MonadBaseControl IO m
       , WithLogger m
       , MonadIO m
       , MonadTimed m
       , MonadMask m
       , MonadDHTDialog SocketState m)
    => KademliaDHTInstance
    -> BaseParams
    -> [ListenerDHT SocketState (KademliaDHT m)]
    -> KademliaDHT m a
    -> m a
runKDHT dhtInstance BaseParams {..} listeners = runKademliaDHT kadConfig
  where
    kadConfig =
      KademliaDHTConfig
      { kdcPort = bpPort
      , kdcListeners = listeners
      , kdcMessageCacheSize = 1000000
      , kdcEnableBroadcast = True
      , kdcNoCacheMessageNames = noCacheMessageNames
      , kdcDHTInstance = dhtInstance
      }

runCH :: MonadIO m
      => NodeParams -> SscNodeContext ssc -> ContextHolder ssc m a -> m a
runCH NodeParams {..} sscNodeContext act =
    flip runContextHolder act . ctx =<<
    (,) <$> liftIO (maybe (pure Nothing) (fmap Just . newMVar) npJLFile) <*>
    liftIO newEmptyMVar
  where
    ctx (jlFile, semaphore) =
        NodeContext
        { ncSystemStart = npSystemStart
        , ncSecretKey = npSecretKey
        , ncTimeLord = npTimeLord
        , ncJLFile = jlFile
        , ncDbPath = npDbPath
        , ncSscContext = sscNodeContext
        , ncPropagation = npPropagation
        , ncBlkSemaphore = semaphore
        }

runTimed :: LoggerName -> Dialog DHTPacking (Transfer SocketState) a -> IO a
runTimed loggerName =
    runTimedIO .
    usingLoggerName loggerName . runTransfer initSocketState . runDialog BiP
  where
    initSocketState :: IO SocketState
    initSocketState = return ()

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

nodeStartMsg :: WithLogger m => BaseParams -> m ()
nodeStartMsg BaseParams {..} = logInfo msg
  where
    msg = sformat ("Started node, joining to DHT network " %build) bpDHTPeers

-- | Get current time as Timestamp. It is intended to be used when you
-- launch the first node. It doesn't make sense in emulation mode.
getCurTimestamp :: IO Timestamp
getCurTimestamp = Timestamp <$> runTimedIO currentTime

setupLoggers :: MonadIO m => LoggingParams -> m ()
setupLoggers LoggingParams{..} = do
    lpLoggerConfig <- readLoggerConfig lpConfigPath
    traverseLoggerConfig (commMapper . dhtMapper) lpLoggerConfig lpHandlerPrefix
  where
    commMapper name | name == "comm" = commLoggerName
                    | otherwise      = name
    dhtMapper  name | name == "dht"  = dhtLoggerName (Proxy :: Proxy (RawRealMode ssc))
                    | otherwise      = name

loggerBracket :: LoggingParams -> IO a -> IO a
loggerBracket lp = bracket_ (setupLoggers lp) releaseAllHandlers

-- | RAII for node starter.
addDevListeners
    :: (MonadDHTDialog SocketState m, MinWorkMode m)
    => Timestamp
    -> [ListenerDHT SocketState m]
    -> [ListenerDHT SocketState m]
addDevListeners sysStart ls =
    if isDevelopment
    then sysStartReqListener sysStart : ls
    else ls

bracketDHTInstance
    :: BaseParams -> (KademliaDHTInstance -> IO a) -> IO a
bracketDHTInstance BaseParams {..} = bracket acquire release
  where
    loggerName = lpRunnerTag bpLoggingParams
    acquire = runTimed loggerName $ startDHTInstance instConfig
    release = runTimed loggerName . stopDHTInstance
    instConfig =
      KademliaDHTInstanceConfig
      { kdcKeyOrType = bpDHTKeyOrType
      , kdcPort = bpPort
      , kdcInitialPeers = nub $ bpDHTPeers ++ defaultPeers
      , kdcExplicitInitial = bpDHTExplicitInitial
      }
