{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

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
       , runKDHT
       , runOurDialog
       , runOurDialogRaw
       ) where

import           Control.Concurrent.MVar      (newEmptyMVar, newMVar, takeMVar,
                                               tryReadMVar)
import           Control.Concurrent.STM.TVar  (TVar, newTVar)
import           Control.Lens                 (each, to, (%~), (^..), (^?), _head, _tail)
import           Control.Monad.Catch          (bracket)
import           Control.Monad.Trans.Control  (MonadBaseControl)
import           Control.Monad.Trans.Resource (runResourceT)
import           Control.TimeWarp.Rpc         (ConnectionPool, Dialog, Transfer,
                                               commLoggerName, runDialog, runTransfer,
                                               runTransferRaw, setForkStrategy)
import           Control.TimeWarp.Timed       (MonadTimed, currentTime, fork, killThread,
                                               repeatForever, runTimedIO, runTimedIO, sec)

import           Data.Default                 (def)
import           Data.List                    (nub)
import qualified Data.Time                    as Time
import           Formatting                   (build, sformat, shown, (%))
import           System.Wlog                  (LoggerName (..), WithLogger, logDebug,
                                               logInfo, logWarning, releaseAllHandlers,
                                               traverseLoggerConfig, usingLoggerName)
import           Universum

import           Pos.Binary                   ()
import           Pos.CLI                      (readLoggerConfig)
import           Pos.Communication            (MutSocketState, SysStartRequest (..),
                                               allListeners, forkStrategy,
                                               newMutSocketState, noCacheMessageNames,
                                               sysStartReqListener,
                                               sysStartReqListenerSlave,
                                               sysStartRespListener)
import           Pos.Constants                (RunningMode (..), defaultPeers,
                                               isDevelopment, runningMode)
import           Pos.Context                  (ContextHolder (..), NodeContext (..),
                                               runContextHolder)
import           Pos.Crypto                   (createProxySecretKey, toPublic)
import           Pos.DB                       (MonadDB (..), initNodeDBs, openNodeDBs,
                                               runDBHolder, _gStateDB)
import           Pos.DB.GState                (getTip)
import           Pos.DB.Misc                  (addProxySecretKey)
import           Pos.Delegation.Class         (runDelegationT)
import           Pos.DHT.Model                (BiP (..), ListenerDHT, MonadDHT (..),
                                               mapListenerDHT, sendToNeighbors)
import           Pos.DHT.Model.Class          (DHTPacking, MonadDHTDialog)
import           Pos.DHT.Real                 (KademliaDHT, KademliaDHTConfig (..),
                                               KademliaDHTInstance,
                                               KademliaDHTInstanceConfig (..),
                                               runKademliaDHT, startDHTInstance,
                                               stopDHTInstance)
import           Pos.Genesis                  (genesisLeaders)
import           Pos.Launcher.Param           (BaseParams (..), LoggingParams (..),
                                               NodeParams (..))
import           Pos.Ssc.Class                (SscConstraint, SscNodeContext, SscParams,
                                               sscCreateNodeContext, sscLoadGlobalState)
import           Pos.Ssc.Extra                (runSscHolder)
import           Pos.Statistics               (getNoStatsT, runStatsT)
import           Pos.Txp.Holder               (runTxpLDHolder)
import qualified Pos.Txp.Types.UtxoView       as UV
import           Pos.Types                    (Timestamp (Timestamp), timestampF)
import           Pos.Update.MemState          (runUSHolder)
import           Pos.Util                     (runWithRandomIntervals)
import           Pos.Util.UserSecret          (peekUserSecret, usKeys, writeUserSecret)
import           Pos.Worker                   (statsWorkers)
import           Pos.WorkMode                 (MinWorkMode, ProductionMode, RawRealMode,
                                               ServiceMode, StatsMode, TimedMode)

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
    -> [ListenerDHT (MutSocketState ssc) (RawRealMode ssc)]
    -> RawRealMode ssc c
    -> IO c
runRawRealMode inst np@NodeParams {..} sscnp listeners action =
    runResourceT $
    do putText $ "Running listeners number: " <> show (length listeners)
       lift $ setupLoggers lp
       initNC <- sscCreateNodeContext @ssc sscnp
       modernDBs <- openNodeDBs npRebuildDb npDbPathM
       -- FIXME: initialization logic must be in scenario.
       runDBHolder modernDBs . runCH np initNC $ initNodeDBs
       initTip <- runDBHolder modernDBs getTip
       initGS <- runDBHolder modernDBs (sscLoadGlobalState @ssc initTip)
       let actionWithMsg = nodeStartMsg npBaseParams >> action
       let kademliazedAction = runKDHT inst npBaseParams listeners actionWithMsg
       let finalAction = setForkStrategy (forkStrategy @ssc) kademliazedAction
       let run =
               runOurDialog newMutSocketState lpRunnerTag .
               runDBHolder modernDBs .
               runCH np initNC .
               flip runSscHolder initGS .
               runTxpLDHolder (UV.createFromDB . _gStateDB $ modernDBs) initTip .
               runDelegationT def .
               runUSHolder $
               finalAction
       lift run
  where
    lp@LoggingParams {..} = bpLoggingParams npBaseParams

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
    -> [ListenerDHT () ServiceMode]
    -> ServiceMode a
    -> IO a
runServiceMode inst bp@BaseParams{..} listeners action = loggerBracket bpLoggingParams $ do
    runOurDialog pass (lpRunnerTag bpLoggingParams) . runKDHT inst bp listeners $
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
       , MonadDHTDialog socketState m)
    => KademliaDHTInstance
    -> BaseParams
    -> [ListenerDHT socketState (KademliaDHT m)]
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

runCH :: (MonadDB ssc m, MonadFail m)
      => NodeParams -> SscNodeContext ssc -> ContextHolder ssc m a -> m a
runCH NodeParams {..} sscNodeContext act = do
    jlFile <- liftIO (maybe (pure Nothing) (fmap Just . newMVar) npJLFile)
    semaphore <- liftIO newEmptyMVar
    lrcSync <- liftIO newEmptyMVar
    userSecret <- peekUserSecret npKeyfilePath

    -- Get primary secret key
    (primarySecretKey, userSecret') <- case npSecretKey of
        Nothing -> case userSecret ^? usKeys . _head of
            Nothing -> fail $ "No secret keys are found in " ++ npKeyfilePath
            Just sk -> return (sk, userSecret)
        Just sk -> do
            let us = userSecret & usKeys %~ (sk :) . filter (/= sk)
            writeUserSecret us
            return (sk, us)

    let eternity = (minBound, maxBound)
        makeOwnPSK = flip (createProxySecretKey primarySecretKey) eternity . toPublic
        ownPSKs = userSecret' ^.. usKeys._tail.each.to makeOwnPSK
    forM_ ownPSKs addProxySecretKey

    userSecretVar <- liftIO . atomically . newTVar $ userSecret'
    let ctx =
            NodeContext
            { ncSystemStart = npSystemStart
            , ncSecretKey = primarySecretKey
            , ncGenesisUtxo = npCustomUtxo
            , ncGenesisLeaders = genesisLeaders npCustomUtxo
            , ncTimeLord = npTimeLord
            , ncJLFile = jlFile
            , ncDbPath = npDbPathM
            , ncSscContext = sscNodeContext
            , ncAttackTypes = npAttackTypes
            , ncAttackTargets = npAttackTargets
            , ncPropagation = npPropagation
            , ncBlkSemaphore = semaphore
            , ncLrcSync = lrcSync
            , ncUserSecret = userSecretVar
            }
    runContextHolder ctx act

runOurDialogRaw
    :: TVar (ConnectionPool socketState)
    -> IO socketState
    -> LoggerName
    -> Dialog DHTPacking (Transfer socketState) a
    -> IO a
runOurDialogRaw cPool ssInitializer loggerName =
    runTimedIO .
    usingLoggerName loggerName . runTransferRaw def cPool ssInitializer . runDialog BiP

runOurDialog
    :: IO socketState
    -> LoggerName
    -> Dialog DHTPacking (Transfer socketState) a
    -> IO a
runOurDialog ssInitializer loggerName =
    runTimedIO .
    usingLoggerName loggerName . runTransfer ssInitializer . runDialog BiP

runTimed :: LoggerName -> TimedMode a -> IO a
runTimed loggerName = runTimedIO . usingLoggerName loggerName

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
    :: (MonadDHTDialog (MutSocketState ssc) m,
        MinWorkMode (MutSocketState ssc) m)
    => Timestamp
    -> [ListenerDHT (MutSocketState ssc) m]
    -> [ListenerDHT (MutSocketState ssc) m]
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
