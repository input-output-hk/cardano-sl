{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | Launcher of full node or simple operations.

module Pos.Launcher
       ( LoggingParams (..)
       , NodeParams (..)
       , BaseParams (..)
       , RealModeRunner
       , getCurTimestamp
       , runNode
       , runNodeReal
       , runNodeStats
       , submitTx
       , submitTxRaw
       , submitTxReal
       , runSupporterReal
       , runTimeSlaveReal
       , runTimeLordReal
       -- Export this for custom usage in CLI utils
       , runServiceMode
       , runRealMode
       , bracketDHTInstance
       ) where

import           Universum

import           Control.Concurrent.MVar     (newEmptyMVar, newMVar, takeMVar,
                                              tryReadMVar)
import           Control.Monad               (fail)
import           Control.Monad.Catch         (bracket)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.TimeWarp.Logging    (LoggerName (..), WithNamedLogger, logDebug,
                                              logError, logInfo, logWarning,
                                              traverseLoggerConfig, usingLoggerName)
import           Control.TimeWarp.Rpc        (BinaryP (..), Dialog, MonadDialog,
                                              NetworkAddress, Transfer, commLoggerName,
                                              runDialog, runTransfer)
import           Control.TimeWarp.Timed      (MonadTimed, currentTime, for, fork, fork_,
                                              killThread, repeatForever, runTimedIO, sec,
                                              sleepForever, wait)
import           Data.Default                (Default)
import           Data.List                   (nub)
import qualified Data.Time                   as Time
import           Formatting                  (build, sformat, shown, (%))
import           System.Log.Logger           (removeAllHandlers)

import           Pos.CLI                     (readLoggerConfig)
import           Pos.Communication           (SysStartRequest (..), allListeners,
                                              noCacheMessageNames, sendTx, statsListeners,
                                              sysStartReqListener, sysStartRespListener)
import           Pos.Constants               (RunningMode (..), defaultPeers,
                                              isDevelopment, runningMode)
import           Pos.Crypto                  (SecretKey, VssKeyPair, hash, sign)
import           Pos.DHT                     (DHTKey, DHTNode (dhtAddr), DHTNodeType (..),
                                              ListenerDHT, MonadDHT (..),
                                              filterByNodeType, mapListenerDHT,
                                              sendToNeighbors)
import           Pos.DHT.Real                (KademliaDHT, KademliaDHTConfig (..),
                                              KademliaDHTInstance,
                                              KademliaDHTInstanceConfig (..),
                                              runKademliaDHT, startDHTInstance,
                                              stopDHTInstance)
import           Pos.Ssc.Class.Listeners     (SscListenersClass)
import           Pos.Ssc.Class.Storage       (SscStorageMode)
import           Pos.Ssc.Class.Types         (SscStorage, SscTypes)
import           Pos.Ssc.Class.Workers       (SscWorkersClass)
import           Pos.State                   (NodeState, openMemState, openState)
import           Pos.State.Storage           (storageFromUtxo)
import           Pos.Statistics              (getNoStatsT, getStatsT)
import           Pos.Types                   (Address, Coin, Timestamp (Timestamp),
                                              Tx (..), TxId, TxIn (..), TxOut (..), Utxo,
                                              timestampF, txF)
import           Pos.Util                    (runWithRandomIntervals)
import           Pos.Worker                  (runWorkers, statsWorkers)
import           Pos.WorkMode                (ContextHolder (..), NodeContext (..),
                                              RealMode, ServiceMode, WorkMode,
                                              getNodeContext, ncPublicKey,
                                              runContextHolder, runDBHolder)

type RealModeSscConstraint ssc =
               (SscTypes ssc, Default (SscStorage ssc),
                SscStorageMode ssc,
                SscListenersClass ssc,
                SscWorkersClass ssc)

type RealModeRunner = KademliaDHTInstance -> NodeParams -> IO ()

-- | Get current time as Timestamp. It is intended to be used when you
-- launch the first node. It doesn't make sense in emulation mode.
getCurTimestamp :: IO Timestamp
getCurTimestamp = Timestamp <$> runTimedIO currentTime

-- | Run full node in any WorkMode.
runNode :: (SscWorkersClass ssc, WorkMode ssc m) => m ()
runNode = do
    pk <- ncPublicKey <$> getNodeContext
    logInfo $ sformat ("My public key is: "%build) pk
    peers <- discoverPeers DHTFull
    logInfo $ sformat ("Known peers: " % build) peers

    waitSystemStart
    runWorkers
    sleepForever

-- | Construct Tx with a single input and single output and send it to
-- the given network addresses.
submitTx :: WorkMode ssc m => [NetworkAddress] -> (TxId, Word32) -> (Address, Coin) -> m Tx
submitTx na (txInHash, txInIndex) (txOutAddress, txOutValue) =
    if null na
      then logError "No addresses to send" >> panic "submitTx failed"
      else do
        sk <- ncSecretKey <$> getNodeContext
        let txOuts = [TxOut {..}]
            txIns = [TxIn {txInSig = sign sk (txInHash, txInIndex, txOuts), ..}]
            tx = Tx {txInputs = txIns, txOutputs = txOuts}
        submitTxRaw na tx
        pure tx

-- | Send the ready-to-use transaction
submitTxRaw :: WorkMode ssc m => [NetworkAddress] -> Tx -> m ()
submitTxRaw na tx = do
    let txId = hash tx
    logInfo $ sformat ("Submitting transaction: "%txF) tx
    logInfo $ sformat ("Transaction id: "%build) txId
    mapM_ (`sendTx` tx) na

-- | Submit tx in real mode.
submitTxReal :: forall ssc . RealModeSscConstraint ssc
             => NodeParams
             -> (TxId, Word32)
             -> (Address, Coin)
             -> IO ()
submitTxReal np input addrCoin = bracketDHTInstance (npBaseParams np) action
  where
    action inst = runRealMode @ssc inst np [] $ do
        peers <- getKnownPeers
        let na = dhtAddr <$> filterByNodeType DHTFull peers
        void $ getNoStatsT $ submitTx na input addrCoin

-- Sanity check in case start time is in future (may happen if clocks
-- are not accurately synchronized, for example).
waitSystemStart :: WorkMode ssc m => m ()
waitSystemStart = do
    Timestamp start <- ncSystemStart <$> getNodeContext
    cur <- currentTime
    when (cur < start) $ wait (for (start - cur))

----------------------------------------------------------------------------
-- Parameters
----------------------------------------------------------------------------

data LoggingParams = LoggingParams
    { lpRunnerTag     :: !LoggerName  -- ^ prefix for logger, like "time-slave"
    , lpHandlerPrefix :: !(Maybe FilePath)
    , lpConfigPath    :: !(Maybe FilePath)
    } deriving (Show)

data BaseParams = BaseParams
    { bpPort               :: !Word16
    , bpDHTPeers           :: ![DHTNode]
    , bpDHTKeyOrType       :: !(Either DHTKey DHTNodeType)
    , bpDHTExplicitInitial :: !Bool
    , bpLoggingParams      :: !LoggingParams
    } deriving (Show)

data NodeParams = NodeParams
    { npDbPath      :: !(Maybe FilePath)
    , npRebuildDb   :: !Bool
    , npSystemStart :: !Timestamp
    , npSecretKey   :: !SecretKey
    , npVssKeyPair  :: !VssKeyPair
    , npBaseParams  :: !BaseParams
    , npCustomUtxo  :: !(Maybe Utxo)
    , npTimeLord    :: !Bool
    , npJLFile      :: !(Maybe FilePath)
    } deriving (Show)


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
         then [sysStartReqListener Nothing, sysStartRespListener mvar]
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

addDevListeners :: NodeParams -> [ListenerDHT (RealMode ssc)] -> [ListenerDHT (RealMode ssc)]
addDevListeners NodeParams{..} ls =
    if isDevelopment
    then sysStartReqListener (Just npSystemStart) : ls
    else ls

-- | Run full node in real mode.
runNodeReal :: forall ssc . RealModeSscConstraint ssc
            => KademliaDHTInstance -> NodeParams -> IO ()
runNodeReal inst np@NodeParams {..} = runRealMode inst np listeners $ getNoStatsT (runNode @ssc)
  where
    listeners = addDevListeners @ssc np noStatsListeners
    noStatsListeners = map (mapListenerDHT getNoStatsT) (allListeners @ssc)

-- | Run full node in benchmarking node
-- TODO: spawn here additional listener, which would accept stat queries
runNodeStats :: forall ssc . RealModeSscConstraint ssc
             => KademliaDHTInstance -> NodeParams -> IO ()
runNodeStats inst np = runRealMode inst np listeners $ getStatsT $ do
    mapM_ fork_ statsWorkers
    runNode @ssc
  where
    listeners = addDevListeners @ssc np sListeners
    sListeners = map (mapListenerDHT getStatsT) $ statsListeners ++ (allListeners @ssc)

----------------------------------------------------------------------------
-- Real mode runners
----------------------------------------------------------------------------

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

-- TODO: use bracket
runRealMode
    :: forall ssc c.
       RealModeSscConstraint ssc
    => KademliaDHTInstance
    -> NodeParams
    -> [ListenerDHT (RealMode ssc)]
    -> RealMode ssc c
    -> IO c
runRealMode inst NodeParams {..} listeners action = do
    setupLoggers lp
    db <- openDb
    runTimed lpRunnerTag . runDBHolder db . runCH . runKDHT inst npBaseParams listeners $
        nodeStartMsg npBaseParams >> action
  where
    lp@LoggingParams{..} = bpLoggingParams npBaseParams
    mStorage = storageFromUtxo <$> npCustomUtxo

    openDb :: IO (NodeState ssc)
    openDb = runTimed lpRunnerTag . runCH $
         maybe (openMemState mStorage)
               (openState mStorage npRebuildDb)
               npDbPath

    runCH :: MonadIO m => ContextHolder m a -> m a
    runCH act = flip runContextHolder act . ctx
                  =<< maybe (pure Nothing) (fmap Just . liftIO . newMVar) npJLFile
      where
        ctx jlFile =
          NodeContext
              { ncSystemStart = npSystemStart
              , ncSecretKey   = npSecretKey
              , ncVssKeyPair  = npVssKeyPair
              , ncTimeLord    = npTimeLord
              , ncJLFile      = jlFile
              }

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
-- Helpers
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

-- TODO: move to log-warper and remove hslogger from dependencies?
loggerBracket :: LoggingParams -> IO a -> IO a
loggerBracket lp = bracket_ (setupLoggers lp) removeAllHandlers

setupLoggers :: MonadIO m => LoggingParams -> m ()
setupLoggers LoggingParams{..} = do
    lpLoggerConfig <- readLoggerConfig lpConfigPath
    traverseLoggerConfig (commMapper . dhtMapper) lpLoggerConfig lpHandlerPrefix
  where
    commMapper name | name == "comm" = commLoggerName
                    | otherwise      = name
    dhtMapper  name | name == "dht"  = dhtLoggerName (Proxy :: Proxy (RealMode ssc))
                    | otherwise      = name

runTimed :: LoggerName -> Dialog BinaryP Transfer a -> IO a
runTimed loggerName =
    runTimedIO .
    usingLoggerName loggerName . runTransfer . runDialog BinaryP

nodeStartMsg :: (WithNamedLogger m, MonadIO m) => BaseParams -> m ()
nodeStartMsg BaseParams {..} = logInfo msg
  where msg = sformat ("Started node, joining to DHT network "%build) bpDHTPeers
