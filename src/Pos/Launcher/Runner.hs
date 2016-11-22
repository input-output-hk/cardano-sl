{-# LANGUAGE AllowAmbiguousTypes   #-}
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
         runRealMode
       , runServiceMode

         -- TODO: describe
       , RealModeRunner
       , runNode
       , runNodeReal
       , runNodeStats
       , runSupporterReal
       , runTimeSlaveReal
       , runTimeLordReal
       -- Export this for custom usage in CLI utils
       , addDevListeners
       , bracketDHTInstance
       ) where

import           Control.Concurrent.MVar     (newEmptyMVar, newMVar, takeMVar,
                                              tryReadMVar)
import           Control.Monad               (fail)
import           Control.Monad.Catch         (bracket)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.TimeWarp.Rpc        (BinaryP (..), Dialog, MonadDialog, Transfer,
                                              commLoggerName, runDialog, runTransfer)
import           Control.TimeWarp.Timed      (MonadTimed, currentTime, fork, fork_,
                                              killThread, repeatForever, runTimedIO, sec)

import           Data.List                   (nub)
import qualified Data.Time                   as Time
import           Formatting                  (build, sformat, shown, (%))
import           System.Directory            (doesDirectoryExist,
                                              removeDirectoryRecursive)
import           System.FilePath             ((</>))
import           System.Log.Logger           (removeAllHandlers)
import           System.Wlog                 (LoggerName (..), WithNamedLogger, logDebug,
                                              logInfo, logWarning, traverseLoggerConfig,
                                              usingLoggerName)
import           Universum

import           Pos.CLI                     (readLoggerConfig)
import           Pos.Communication           (SysStartRequest (..), allListeners,
                                              noCacheMessageNames, statsListeners,
                                              sysStartReqListener,
                                              sysStartReqListenerSlave,
                                              sysStartRespListener)
import           Pos.Constants               (RunningMode (..), defaultPeers,
                                              isDevelopment, runningMode)
import           Pos.DHT                     (ListenerDHT, MonadDHT (..), mapListenerDHT,
                                              sendToNeighbors)
import           Pos.DHT.Real                (KademliaDHT, KademliaDHTConfig (..),
                                              KademliaDHTInstance,
                                              KademliaDHTInstanceConfig (..),
                                              runKademliaDHT, startDHTInstance,
                                              stopDHTInstance)
import           Pos.Launcher.Param          (BaseParams (..), LoggingParams (..),
                                              NodeParams (..))
import           Pos.Launcher.Scenario       (runNode)
import           Pos.Ssc.Class               (SscConstraint)
import           Pos.State                   (NodeState, openMemState, openState)
import           Pos.State.Storage           (storageFromUtxo)
import           Pos.Statistics              (getNoStatsT, getStatsT)
import           Pos.Types                   (Timestamp (Timestamp), timestampF)
import           Pos.Util                    (runWithRandomIntervals)
import           Pos.Worker                  (statsWorkers)
import           Pos.WorkMode                (ContextHolder (..), NodeContext (..),
                                              RealMode, ServiceMode, runContextHolder,
                                              runDBHolder, runSscLDImpl)

type RealModeRunner = KademliaDHTInstance -> NodeParams -> IO ()

----------------------------------------------------------------------------
-- Service node runners
----------------------------------------------------------------------------

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

-----------------------------------------------------------------------------
-- Main launchers
-----------------------------------------------------------------------------

-- | Run full node in real mode.
runNodeReal :: forall ssc . SscConstraint ssc
            => KademliaDHTInstance -> NodeParams -> IO ()
runNodeReal inst np = runRealMode inst np listeners $ getNoStatsT (runNode @ssc)
  where
    listeners = addDevListeners @ssc np noStatsListeners
    noStatsListeners = map (mapListenerDHT getNoStatsT) (allListeners @ssc)

-- | Run full node in benchmarking node
-- TODO: spawn here additional listener, which would accept stat queries
runNodeStats :: forall ssc . SscConstraint ssc
             => KademliaDHTInstance -> NodeParams -> IO ()
runNodeStats inst np = runRealMode inst np listeners $ getStatsT $ do
    mapM_ fork_ statsWorkers
    runNode @ssc
  where
    listeners = addDevListeners @ssc np sListeners
    sListeners = map (mapListenerDHT getStatsT) $ statsListeners ++ (allListeners @ssc)

----------------------------------------------------------------------------
-- High level runners
----------------------------------------------------------------------------

-- TODO: use bracket
runRealMode
    :: forall ssc c.
       SscConstraint ssc
    => KademliaDHTInstance
    -> NodeParams
    -> [ListenerDHT (RealMode ssc)]
    -> RealMode ssc c
    -> IO c
runRealMode inst np@NodeParams {..} listeners action = do
    setupLoggers lp
    db <- openDb
    runTimed lpRunnerTag .
        runDBHolder db .
        runCH np . runSscLDImpl . runKDHT inst npBaseParams listeners $
        nodeStartMsg npBaseParams >> action
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
        runTimed lpRunnerTag . runCH np $
            maybe
                (openMemState mStorage)
                (openState mStorage False)
                ((</> "main") <$> npDbPath)

runServiceMode
    :: KademliaDHTInstance
    -> BaseParams
    -> [ListenerDHT ServiceMode]
    -> ServiceMode a
    -> IO a
runServiceMode inst bp@BaseParams{..} listeners action = loggerBracket bpLoggingParams $ do
    runTimed (lpRunnerTag bpLoggingParams) . runKDHT inst bp listeners $
        nodeStartMsg bp >> action

----------------------------------------------------------------------------
-- Lower level runners
----------------------------------------------------------------------------

runKDHT
    :: (MonadBaseControl IO m
       ,WithNamedLogger m
       ,MonadIO m
       ,MonadTimed m
       ,MonadMask m
       ,MonadDialog BinaryP m)
    => KademliaDHTInstance
    -> BaseParams
    -> [ListenerDHT (KademliaDHT m)]
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

runCH :: MonadIO m => NodeParams -> ContextHolder m a -> m a
runCH NodeParams {..} act =
    flip runContextHolder act . ctx =<<
    maybe (pure Nothing) (fmap Just . liftIO . newMVar) npJLFile
  where
    ctx jlFile =
        NodeContext
        { ncSystemStart = npSystemStart
        , ncSecretKey = npSecretKey
        , ncVssKeyPair = npVssKeyPair
        , ncTimeLord = npTimeLord
        , ncJLFile = jlFile
        , ncDbPath = npDbPath
        }

runTimed :: LoggerName -> Dialog BinaryP Transfer a -> IO a
runTimed loggerName =
    runTimedIO .
    usingLoggerName loggerName . runTransfer . runDialog BinaryP

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

nodeStartMsg :: (WithNamedLogger m, MonadIO m) => BaseParams -> m ()
nodeStartMsg BaseParams {..} = logInfo msg
  where
    msg = sformat ("Started node, joining to DHT network " %build) bpDHTPeers

getCurTimestamp :: IO Timestamp
getCurTimestamp = Timestamp <$> runTimedIO currentTime

setupLoggers :: MonadIO m => LoggingParams -> m ()
setupLoggers LoggingParams{..} = do
    lpLoggerConfig <- readLoggerConfig lpConfigPath
    traverseLoggerConfig (commMapper . dhtMapper) lpLoggerConfig lpHandlerPrefix
  where
    commMapper name | name == "comm" = commLoggerName
                    | otherwise      = name
    dhtMapper  name | name == "dht"  = dhtLoggerName (Proxy :: Proxy (RealMode ssc))
                    | otherwise      = name

-- TODO: move to log-warper and remove hslogger from dependencies?
loggerBracket :: LoggingParams -> IO a -> IO a
loggerBracket lp = bracket_ (setupLoggers lp) removeAllHandlers

addDevListeners :: NodeParams -> [ListenerDHT (RealMode ssc)] -> [ListenerDHT (RealMode ssc)]
addDevListeners NodeParams{..} ls =
    if isDevelopment
    then sysStartReqListener npSystemStart : ls
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
