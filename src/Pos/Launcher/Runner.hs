{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Runners in various modes.

module Pos.Launcher.Runner
       ( -- * High level runners
         runRawRealMode
       , runProductionMode
       , runStatsMode
       , runServiceMode

       --  -- * Service runners
       , runTimeSlaveReal
       , runTimeLordReal

       -- * Exported for custom usage in CLI utils
       , addDevListeners
       , setupLoggers
       , bracketDHTInstance
       , runServer
       , loggerBracket
       ) where

import           Control.Concurrent.MVar      (newEmptyMVar, newMVar, takeMVar,
                                               tryReadMVar)
import           Control.Concurrent.STM.TVar  (TVar, newTVar)
import           Control.Lens                 (each, to, (%~), (^..), (^?), _head, _tail)
import qualified Control.Monad.Catch          as MonadCatch
import           Control.Monad.Fix            (MonadFix)
import           Control.Monad.Trans.Control  (MonadBaseControl)
import           Control.Monad.Trans.Resource (runResourceT)
import           Control.TimeWarp.Rpc         (ConnectionPool, Dialog, Transfer,
                                               commLoggerName, runDialog, runTransfer,
                                               runTransferRaw, setForkStrategy)
import           Control.TimeWarp.Timed       (MonadTimed, runTimedIO, runTimedIO, sec)
import           Data.Default                 (def)
import           Data.List                    (nub)
import qualified Data.Time                    as Time
import           Formatting                   (build, sformat, shown, (%))
import           Mockable                     (MonadMockable, Production (..), bracket,
                                               currentTime, fork, killThread, throw)
import           Network.Transport.Concrete   (concrete)
import qualified Network.Transport.TCP        as TCP
import           Node                         (Listener, NodeAction (..), SendActions,
                                               hoistSendActions, node)
import qualified STMContainers.Map            as SM
import           System.Random                (newStdGen)
import           System.Wlog                  (LoggerName (..), WithLogger, logDebug,
                                               logError, logInfo, logWarning,
                                               releaseAllHandlers, traverseLoggerConfig,
                                               usingLoggerName)
import           Universum                    hiding (bracket)

import           Pos.Binary                   ()
import           Pos.CLI                      (readLoggerConfig)
import           Pos.Communication            (BiP (..), SysStartRequest (..),
                                               allListeners, forkStrategy, newStateHolder,
                                               noCacheMessageNames, sysStartReqListener,
                                               sysStartReqListenerSlave,
                                               sysStartRespListener)
import           Pos.Constants                (defaultPeers, isDevelopment, runningMode)
import qualified Pos.Constants                as Const
import           Pos.Context                  (ContextHolder (..), NodeContext (..),
                                               runContextHolder)
import           Pos.Crypto                   (createProxySecretKey, toPublic)
import qualified Pos.DB                       as Modern
import           Pos.DB.Misc                  (addProxySecretKey)
import           Pos.Delegation.Class         (runDelegationT)
import           Pos.Launcher.Param           (BaseParams (..), LoggingParams (..),
                                               NodeParams (..))
import           Pos.NewDHT.Model             (MonadDHT (..), sendToNeighbors)
import           Pos.NewDHT.Real              (KademliaDHT, KademliaDHTInstance,
                                               KademliaDHTInstanceConfig (..),
                                               runKademliaDHT, startDHTInstance,
                                               stopDHTInstance)

import           Pos.Ssc.Class                (SscConstraint, SscNodeContext, SscParams,
                                               sscCreateNodeContext, sscLoadGlobalState)
import           Pos.Ssc.Extra                (runSscHolder)
import           Pos.Statistics               (getNoStatsT, runStatsT')
import           Pos.Txp.Holder               (runTxpLDHolder)
import qualified Pos.Txp.Types.UtxoView       as UV
import           Pos.Types                    (Timestamp (Timestamp), timestampF)
import           Pos.Util                     (runWithRandomIntervals')
import           Pos.Util.UserSecret          (peekUserSecret, usKeys, writeUserSecret)
import           Pos.Worker                   (statsWorkers)
import           Pos.WorkMode                 (MinWorkMode, NewMinWorkMode,
                                               ProductionMode, RawRealMode, ServiceMode,
                                               StatsMode)

----------------------------------------------------------------------------
-- Service node runners
----------------------------------------------------------------------------

-- | Runs node as time-slave inside IO monad.
runTimeSlaveReal :: KademliaDHTInstance -> BaseParams -> IO Timestamp
runTimeSlaveReal inst bp = do
    mvar <- liftIO newEmptyMVar
    runServiceMode inst bp (listeners mvar) $ \sendActions ->
      case runningMode of
         Const.Development -> do
           tId <- fork $
             runWithRandomIntervals' (sec 10) (sec 60) $ liftIO (tryReadMVar mvar) >>= \case
                 Nothing -> do
                    logInfo "Asking neighbors for system start"
                    sendToNeighbors sendActions SysStartRequest `catchAll`
                       \e -> logDebug $ sformat
                       ("Error sending SysStartRequest to neighbors: " % shown) e
                 Just _ -> fail "Close thread"
           t <- liftIO $ takeMVar mvar
           killThread tId
           t <$ logInfo (sformat ("[Time slave] adopted system start " % timestampF) t)
         Const.Production ts -> logWarning "Time slave launched in Production" $> ts
  where
    listeners mvar =
      if isDevelopment
         then [sysStartReqListenerSlave, sysStartRespListener mvar]
         else []

-- | Runs time-lord to acquire system start.
runTimeLordReal :: LoggingParams -> IO Timestamp
runTimeLordReal lp@LoggingParams{..} = loggerBracket lp $ do
    t <- runProduction $ Timestamp <$> currentTime
    usingLoggerName lpRunnerTag (doLog t) $> t
  where
    doLog t = do
        realTime <- liftIO Time.getZonedTime
        logInfo (sformat ("[Time lord] System start: " %timestampF%", i. e.: "%shown) t realTime)

------------------------------------------------------------------------------
---- High level runners
------------------------------------------------------------------------------

-- | RawRealMode runner.
runRawRealMode
    :: forall ssc c.
       SscConstraint ssc
    => KademliaDHTInstance
    -> NodeParams
    -> SscParams ssc
    -> [Listener BiP (RawRealMode ssc)]
    -> (SendActions BiP (RawRealMode ssc) -> RawRealMode ssc c)
    -> IO c
runRawRealMode inst np@NodeParams {..} sscnp listeners action =
    loggerBracket lp .
    runProduction .
    usingLoggerName lpRunnerTag $ do
       modernDBs <- Modern.openNodeDBs npRebuildDb npDbPathM npCustomUtxo
       initTip <- Modern.runDBHolder modernDBs Modern.getTip
       initGS <- Modern.runDBHolder modernDBs (sscLoadGlobalState @ssc initTip)
       initNC <- sscCreateNodeContext @ssc sscnp
       Modern.runDBHolder modernDBs .
          runCH np initNC .
          flip runSscHolder initGS .
          runTxpLDHolder (UV.createFromDB . Modern._utxoDB $ modernDBs) initTip .
          runDelegationT def .
          runKademliaDHT inst .
          runServer (bpPort npBaseParams) listeners $
              \sa -> nodeStartMsg npBaseParams >> action sa
  where
    lp@LoggingParams {..} = bpLoggingParams npBaseParams

-- | ServiceMode runner.
runServiceMode
    :: KademliaDHTInstance
    -> BaseParams
    -> [Listener BiP ServiceMode]
    -> (SendActions BiP ServiceMode -> ServiceMode a)
    -> IO a
runServiceMode inst bp@BaseParams{..} listeners action =
    loggerBracket bpLoggingParams .
    runProduction .
    usingLoggerName (lpRunnerTag bpLoggingParams) .
    runKademliaDHT inst .
    runServer bpPort listeners $
        \sa -> nodeStartMsg bp >> action sa

runServer :: (MonadIO m, MonadMockable m, WithLogger m, MonadFix m)
  => Word16 -> [Listener BiP m] -> (SendActions BiP m -> m b) -> m b
runServer port listeners action = do
    transportE <- liftIO $ TCP.createTransport
                             "0.0.0.0"
                             (show port)
                             TCP.defaultTCPParameters
    case transportE of
      Left e -> do
          logError $ sformat ("Error creating TCP transport: " % shown) e
          throw e
      Right transport -> do
          stdGen <- liftIO newStdGen
          node (concrete transport) stdGen BiP $ \__node ->
              pure $ NodeAction listeners action

-- | ProductionMode runner.
runProductionMode
    :: forall ssc a.
       SscConstraint ssc
    => KademliaDHTInstance
    -> NodeParams
    -> SscParams ssc
    -> (SendActions BiP (ProductionMode ssc) -> ProductionMode ssc a)
    -> IO a
runProductionMode inst np@NodeParams {..} sscnp action =
    runRawRealMode inst np sscnp listeners $
        \sendActions -> getNoStatsT . action $ hoistSendActions lift getNoStatsT sendActions
  where
    listeners = addDevListeners npSystemStart []
    -- TODO [CSL-447] Uncomment
    --noStatsListeners = map (mapListenerDHT getNoStatsT) (allListeners @ssc)

-- | StatsMode runner.
-- [CSL-169]: spawn here additional listener, which would accept stat queries
-- can be done as part of refactoring (or someone who will refactor will create new issue).
runStatsMode
    :: forall ssc a.
       SscConstraint ssc
    => KademliaDHTInstance
    -> NodeParams
    -> SscParams ssc
    -> (SendActions BiP (StatsMode ssc) -> StatsMode ssc a)
    -> IO a
runStatsMode inst np@NodeParams {..} sscnp action = do
    -- [CSL-447] TODO uncomment
    --mapM_ fork statsWorkers
    runRawRealMode inst np sscnp listeners $
        \sendActions -> do
            statMap <- liftIO SM.newIO
            runStatsT' statMap . action $ hoistSendActions lift (runStatsT' statMap) sendActions
  where
    listeners = addDevListeners npSystemStart []
    -- TODO [CSL-447] Uncomment
    --sListeners = map (mapListenerDHT runStatsT) $ allListeners @ssc

----------------------------------------------------------------------------
-- Lower level runners
----------------------------------------------------------------------------

runCH :: (Modern.MonadDB ssc m, MonadFail m)
      => NodeParams -> SscNodeContext ssc -> ContextHolder ssc m a -> m a
runCH NodeParams {..} sscNodeContext act = do
    jlFile <- liftIO (maybe (pure Nothing) (fmap Just . newMVar) npJLFile)
    semaphore <- liftIO newEmptyMVar
    sscRichmen <- liftIO newEmptyMVar
    sscLeaders <- liftIO newEmptyMVar
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
            , ncTimeLord = npTimeLord
            , ncJLFile = jlFile
            , ncDbPath = npDbPathM
            , ncSscContext = sscNodeContext
            , ncAttackTypes = npAttackTypes
            , ncAttackTargets = npAttackTargets
            , ncPropagation = npPropagation
            , ncBlkSemaphore = semaphore
            , ncSscRichmen = sscRichmen
            , ncSscLeaders = sscLeaders
            , ncUserSecret = userSecretVar
            }
    runContextHolder ctx act

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

nodeStartMsg :: WithLogger m => BaseParams -> m ()
nodeStartMsg BaseParams {..} = logInfo msg
  where
    msg = sformat ("Started node, joining to DHT network " %build) bpDHTPeers

setupLoggers :: MonadIO m => LoggingParams -> m ()
setupLoggers LoggingParams{..} = do
    lpLoggerConfig <- readLoggerConfig lpConfigPath
    traverseLoggerConfig (commMapper . dhtMapper) lpLoggerConfig lpHandlerPrefix
  where
    commMapper name | name == "comm" = commLoggerName
                    | otherwise      = name
    dhtMapper  name | name == "dht"  = dhtLoggerName (Proxy :: Proxy (RawRealMode ssc))
                    | otherwise      = name

-- | RAII for node starter.
loggerBracket :: LoggingParams -> IO a -> IO a
loggerBracket lp = bracket_ (setupLoggers lp) releaseAllHandlers

addDevListeners
    :: (NewMinWorkMode m)
    => Timestamp
    -> [Listener BiP m]
    -> [Listener BiP m]
addDevListeners sysStart ls =
    if isDevelopment
    then sysStartReqListener sysStart : ls
    else ls

-- [CSL-447] Make all launchers use Production instead of IO and replace types here
bracketDHTInstance
    :: BaseParams -> (KademliaDHTInstance -> IO a) -> IO a
bracketDHTInstance BaseParams {..} action = runProduction $ bracket acquire release (Production . action)
  where
    loggerName = lpRunnerTag bpLoggingParams
    acquire = usingLoggerName loggerName $ startDHTInstance instConfig
    release = usingLoggerName loggerName . stopDHTInstance
    instConfig =
        KademliaDHTInstanceConfig
        { kdcKeyOrType = bpDHTKeyOrType
        , kdcPort = bpPort
        , kdcInitialPeers = nub $ bpDHTPeers ++ defaultPeers
        , kdcExplicitInitial = bpDHTExplicitInitial
        }
