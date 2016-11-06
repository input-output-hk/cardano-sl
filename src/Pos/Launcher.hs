{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Launcher of full node or simple operations.

module Pos.Launcher
       ( LoggingParams (..)
       , NodeParams (..)
       , BaseParams (..)
       , getCurTimestamp
       , runNode
       , runNodeReal
       , runNodeStats
       , submitTx
       , submitTxReal
       , runSupporterReal
       , runTimeSlaveReal
       , runTimeLordReal
       -- Export this for custom usage in CLI utils
       , runServiceMode
       , runRealMode
       , bracketDHTInstance
       ) where


import           Control.Concurrent.MVar     (newEmptyMVar, takeMVar, tryReadMVar)
import           Control.Monad               (fail)
import           Control.Monad.Catch         (bracket)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.TimeWarp.Logging    (LoggerName, Severity (Warning),
                                              WithNamedLogger, initLogging, logDebug,
                                              logError, logInfo, logWarning, setSeverity,
                                              setSeverityMaybe, usingLoggerName)
import           Control.TimeWarp.Rpc        (BinaryP (..), Dialog, MonadDialog,
                                              NetworkAddress, Transfer, runDialog,
                                              runTransfer)
import           Control.TimeWarp.Timed      (MonadTimed, currentTime, for, fork, fork_,
                                              killThread, ms, repeatForever, runTimedIO,
                                              sec, sleepForever, wait, wait)
import           Data.Default                (Default (def))
import qualified Data.Time                   as Time
import           Formatting                  (build, sformat, shown, (%))
import           Universum

import           Pos.Communication           (SysStartRequest (..), SysStartResponse (..),
                                              allListeners, noCacheMessageNames, sendTx,
                                              serverLoggerName, statsListener,
                                              sysStartReqListener, sysStartRespListener)
import           Pos.Constants               (RunningMode (..), isDevelopment,
                                              runningMode)
import           Pos.Crypto                  (SecretKey, VssKeyPair, hash, sign)
import           Pos.DHT                     (DHTKey, DHTNode (dhtAddr), DHTNodeType (..),
                                              ListenerDHT, MonadDHT (..),
                                              filterByNodeType, mapListenerDHT,
                                              sendToNeighbors, sendToNetwork)
import           Pos.DHT.Real                (KademliaDHT, KademliaDHTConfig (..),
                                              KademliaDHTInstance,
                                              KademliaDHTInstanceConfig (..),
                                              runKademliaDHT, startDHTInstance,
                                              stopDHTInstance)
import           Pos.State                   (NodeState, openMemState, openState)
import           Pos.State.Storage           (storageFromUtxo)
import           Pos.Statistics              (getNoStatsT, getStatsT)
import           Pos.Types                   (Address, Coin, Timestamp (Timestamp),
                                              Tx (..), TxId, TxIn (..), TxOut (..), Utxo,
                                              timestampF, txF)
import           Pos.Util                    (runWithRandomIntervals)
import           Pos.Worker                  (runWorkers)
import           Pos.WorkMode                (ContextHolder (..), DBHolder (..),
                                              NodeContext (..), RealMode, ServiceMode,
                                              WorkMode, getNodeContext, ncPublicKey,
                                              ncSecretKey)

-- | Get current time as Timestamp. It is intended to be used when you
-- launch the first node. It doesn't make sense in emulation mode.
getCurTimestamp :: IO Timestamp
getCurTimestamp = Timestamp <$> runTimedIO currentTime

-- | Run full node in any WorkMode.
runNode :: WorkMode m => m ()
runNode = do
    pk <- ncPublicKey <$> getNodeContext
    logInfo $ sformat ("My public key is: "%build) pk
    whenM (ncTimeLord <$> getNodeContext) $
      ncSystemStart <$> getNodeContext
          >>= \(SysStartResponse . Just -> mT) -> fork_ $
            runWithRandomIntervals (ms 500) (sec 5) $ do
              logInfo "Broadcasting system start"
              sendToNetwork mT
    peers <- discoverPeers DHTFull
    logInfo $ sformat ("Known peers: " % build) peers

    waitSystemStart
    runWorkers
    sleepForever

-- | Construct Tx with a single input and single output and send it to
-- the given network addresses.
submitTx :: WorkMode m => [NetworkAddress] -> (TxId, Word32) -> (Address, Coin) -> m ()
submitTx na (txInHash, txInIndex) (txOutAddress, txOutValue) =
    if null na
      then logError "No addresses to send"
      else do
        sk <- ncSecretKey <$> getNodeContext
        let txOuts = [TxOut {..}]
            txIns = [TxIn {txInSig = sign sk (txInHash, txInIndex, txOuts), ..}]
            tx = Tx {txInputs = txIns, txOutputs = txOuts}
            txId = hash tx
        logInfo $ sformat ("Submitting transaction: "%txF) tx
        logInfo $ sformat ("Transaction id: "%build) txId
        mapM_ (`sendTx` tx) na

-- | Submit tx in real mode.
submitTxReal :: NodeParams
             -> (TxId, Word32)
             -> (Address, Coin)
             -> IO ()
submitTxReal np input addrCoin = bracketDHTInstance (npBaseParams np) action
  where
    action inst = runRealMode inst np [] $ do
                peers <- getKnownPeers
                let na = dhtAddr <$> filterByNodeType DHTFull peers
                getNoStatsT $ submitTx na input addrCoin

-- Sanity check in case start time is in future (may happen if clocks
-- are not accurately synchronized, for example).
waitSystemStart :: WorkMode m => m ()
waitSystemStart = do
    Timestamp start <- ncSystemStart <$> getNodeContext
    cur <- currentTime
    when (cur < start) $ wait (for (start - cur))

----------------------------------------------------------------------------
-- Parameters
----------------------------------------------------------------------------

data LoggingParams = LoggingParams
    { lpRootLogger     :: !LoggerName
    , lpMainSeverity   :: !Severity
    , lpDhtSeverity    :: !(Maybe Severity)
    , lpServerSeverity :: !(Maybe Severity)
    , lpCommSeverity   :: !(Maybe Severity)
    , lpWorkerSeverity :: !(Maybe Severity)
    } deriving (Show)

instance Default LoggingParams where
    def =
        LoggingParams
        { lpRootLogger     = mempty
        , lpMainSeverity   = Warning
        , lpDhtSeverity    = Nothing
        , lpServerSeverity = Nothing
        , lpCommSeverity   = Nothing
        , lpWorkerSeverity = Nothing
        }

-- | Parameters necessary to run node.
data NodeParams = NodeParams
    { npDbPath      :: !(Maybe FilePath)
    , npRebuildDb   :: !Bool
    , npSecretKey   :: !SecretKey
    , npVssKeyPair  :: !VssKeyPair
    , npBaseParams  :: !BaseParams
    , npSystemStart :: !Timestamp
    , npCustomUtxo  :: !(Maybe Utxo)
    , npTimeLord    :: !Bool
    } deriving (Show)

data BaseParams = BaseParams
    { bpLogging            :: !LoggingParams
    , bpPort               :: !Word16
    , bpDHTPeers           :: ![DHTNode]
    , bpDHTKeyOrType       :: !(Either DHTKey DHTNodeType)
    , bpDHTExplicitInitial :: !Bool
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
             runWithRandomIntervals (sec 10) (sec 60) $ do
               mT <- liftIO $ tryReadMVar mvar
               case mT of
                 Nothing -> do
                    logInfo "Asking neighbors for system start"
                    (void $ sendToNeighbors SysStartRequest) `catchAll`
                       \e -> logDebug $ sformat ("Error sending SysStartRequest to neighbors: " % shown) e
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
runTimeLordReal lp = do
    setupLoggingReal lp
    t <- getCurTimestamp
    usingLoggerName (lpRootLogger lp) (doLog t) $> t
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

addDevListeners :: NodeParams -> [ListenerDHT RealMode] -> [ListenerDHT RealMode]
addDevListeners NodeParams {..} ls =
    if isDevelopment
    then sysStartReqListener (Just npSystemStart) : ls
    else ls

-- | Run full node in real mode.
runNodeReal :: KademliaDHTInstance -> NodeParams -> IO ()
runNodeReal inst np@NodeParams {..} = runRealMode inst np listeners $ getNoStatsT runNode
  where
    listeners = addDevListeners np noStatsListeners
    noStatsListeners = map (mapListenerDHT getNoStatsT) allListeners

-- | Run full node in benchmarking node
-- TODO: spawn here additional listener, which would accept stat queries
runNodeStats :: KademliaDHTInstance -> NodeParams -> IO ()
runNodeStats inst np = runRealMode inst np listeners $ getStatsT runNode
  where
    listeners = addDevListeners np statsListeners
    statsListeners = map (mapListenerDHT getStatsT) $ statsListener : allListeners

----------------------------------------------------------------------------
-- Real mode runners
----------------------------------------------------------------------------

bracketDHTInstance
    :: BaseParams -> (KademliaDHTInstance -> IO a) -> IO a
bracketDHTInstance BaseParams {..} = bracket acquire release
  where
    logger = lpRootLogger bpLogging
    acquire = runTimed logger $ startDHTInstance instConfig
    release = runTimed logger . stopDHTInstance
    instConfig =
      KademliaDHTInstanceConfig
      { kdcKeyOrType = bpDHTKeyOrType
      , kdcPort = bpPort
      , kdcInitialPeers = bpDHTPeers
      , kdcExplicitInitial = bpDHTExplicitInitial
      }

-- TODO: use bracket
runRealMode :: KademliaDHTInstance -> NodeParams -> [ListenerDHT RealMode] -> RealMode a -> IO a
runRealMode inst NodeParams {..} listeners action = do
    setupLoggingReal logParams
    db <- openDb
    runTimed loggerName . runDBH db . runCH . runKDHT inst npBaseParams listeners $
        nodeStartMsg npBaseParams >> action
  where
    logParams  = bpLogging npBaseParams
    loggerName = lpRootLogger logParams
    mStorage = storageFromUtxo <$> npCustomUtxo

    openDb :: IO NodeState
    openDb = runTimed loggerName . runCH $
         maybe (openMemState mStorage)
               (openState mStorage npRebuildDb)
               npDbPath

    runDBH :: NodeState -> DBHolder m a -> m a
    runDBH db = flip runReaderT db . getDBHolder

    runCH :: ContextHolder m a -> m a
    runCH = flip runReaderT ctx . getContextHolder
      where
        ctx = NodeContext
              { ncSystemStart = npSystemStart
              , ncSecretKey = npSecretKey
              , ncVssKeyPair = npVssKeyPair
              , ncTimeLord = npTimeLord
              }

runServiceMode :: KademliaDHTInstance -> BaseParams -> [ListenerDHT ServiceMode] -> ServiceMode a -> IO a
runServiceMode inst bp@BaseParams {..} listeners action = do
    setupLoggingReal bpLogging
    runTimed (lpRootLogger bpLogging) . runKDHT inst bp listeners $
        nodeStartMsg bp >> action

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

runKDHT :: (MonadBaseControl IO m, WithNamedLogger m, MonadIO m, MonadTimed m, MonadMask m, MonadDialog BinaryP m)
        => KademliaDHTInstance -> BaseParams -> [ListenerDHT (KademliaDHT m)] -> KademliaDHT m a -> m a
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

runTimed :: LoggerName -> Dialog BinaryP Transfer a -> IO a
runTimed loggerName =
    runTimedIO .
    usingLoggerName loggerName . runTransfer . runDialog BinaryP

setupLoggingReal :: LoggingParams -> IO ()
setupLoggingReal LoggingParams {..} = do
    initLogging Warning
    setSeverity lpRootLogger lpMainSeverity
    setSeverityMaybe
        (lpRootLogger <> dhtLoggerName (Proxy :: Proxy RealMode))
        lpDhtSeverity
    -- TODO: `comm` shouldn't be hardcoded, it should be taken
    -- from MonadTransfer or something
    setSeverityMaybe (lpRootLogger <> "comm") lpCommSeverity
    setSeverityMaybe (lpRootLogger <> serverLoggerName) lpServerSeverity

nodeStartMsg :: (WithNamedLogger m, MonadIO m) => BaseParams -> m ()
nodeStartMsg BaseParams {..} = logInfo msg
  where msg = sformat ("Started node, joining to DHT network "%build) bpDHTPeers
