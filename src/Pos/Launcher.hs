{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Launcher of full node or simple operations.

module Pos.Launcher
       ( LoggingParams (..)
       , NodeParams (..)
       , BaseParams (..)
       , getCurTimestamp
       , runNode
       , runNodeReal
       , runNodeBench
       , submitTx
       , submitTxReal
       , runSupporterReal
       , runTimeSlaveReal
       , runTimeLordReal
       ) where

import           Control.Concurrent.MVar  (newEmptyMVar, takeMVar)
import           Control.TimeWarp.Logging (LoggerName, Severity (Warning),
                                           WithNamedLogger, initLogging, logError,
                                           logInfo, logWarning, setSeverity,
                                           setSeverityMaybe, usingLoggerName)
import           Control.TimeWarp.Rpc     (BinaryDialog, MonadDialog, NetworkAddress,
                                           Transfer, runBinaryDialog, runTransfer)
import           Control.TimeWarp.Timed   (MonadTimed, currentTime, fork, killThread,
                                           repeatForever, runTimedIO, sec, sleepForever)
import           Data.Default             (Default (def))
import           Formatting               (build, sformat, (%))
import           Universum                hiding (catch, killThread)

import           Pos.Communication        (SysStartRequest (..), allListeners, sendTx,
                                           sysStartMessageNames, sysStartReqListener,
                                           sysStartRespListener)
import           Pos.Constants            (RunningMode (..), isDevelopment, runningMode)
import           Pos.Crypto               (SecretKey, VssKeyPair, hash, sign)
import           Pos.DHT                  (DHTKey, DHTNode (dhtAddr), DHTNodeType (..),
                                           ListenerDHT, MonadDHT (..), filterByNodeType,
                                           sendToNeighbors)
import           Pos.DHT.Real             (KademliaDHT, KademliaDHTConfig (..),
                                           runKademliaDHT)
import           Pos.State                (NodeState, openMemState, openState)
import           Pos.Types                (Address, Coin, Timestamp (Timestamp), Tx (..),
                                           TxId, TxIn (..), TxOut (..), timestampF, txF)
import           Pos.Worker               (runWorkers)
import           Pos.WorkMode             (BenchMode, BenchmarkT (..), ContextHolder (..),
                                           DBHolder (..), NoBenchmarkT (..),
                                           NodeContext (..), RealMode, RealWithoutNetwork,
                                           SemiRealMode, WorkIOMode, WorkMode,
                                           getNodeContext, ncSecretKey)

-- | Get current time as Timestamp. It is intended to be used when you
-- launch the first node. It doesn't make sense in emulation mode.
getCurTimestamp :: IO Timestamp
getCurTimestamp = Timestamp <$> runTimedIO currentTime

-- | Run full node in any WorkMode.
runNode :: WorkMode m => m ()
runNode = do
    peers <- discoverPeers DHTFull
    logInfo $ sformat ("Known peers: " % build) peers

    runWorkers
    sleepForever


-- | Run full node in benchmarking node
-- TODO: spawn here additional listener, which would accept stat queries
runNodeBench :: NodeParams -> IO ()
runNodeBench p = runBenchMode p allListeners runNode

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
        mapM_ (flip sendTx tx) na

-- | Submit tx in real mode.
submitTxReal :: NodeParams
             -> (TxId, Word32)
             -> (Address, Coin)
             -> IO ()
submitTxReal np input addrCoin = runRealMode np [] action
  where action = (fmap dhtAddr . filterByNodeType DHTFull) <$> getKnownPeers >>= \na -> submitTx na input addrCoin

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
    } deriving (Show)

data BaseParams = BaseParams
    { bpLogging      :: !LoggingParams
    , bpPort         :: !Word16
    , bpDHTPeers     :: ![DHTNode]
    , bpDHTKeyOrType :: !(Either DHTKey DHTNodeType)
    } deriving (Show)


runTimeSlaveReal :: BaseParams -> IO Timestamp
runTimeSlaveReal bp = do
    mvar <- liftIO newEmptyMVar
    runRealMode' bp return (listeners mvar) $ do
      case runningMode of
         Development -> do
           tId <- fork $ do
             repeatForever (sec 5) (const . return $ sec 5) $ do
               logInfo "Asking neighbors for system start"
               void $ sendToNeighbors SysStartRequest
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
    runTimed (lpRootLogger lp) $ do
        t <- Timestamp <$> currentTime
        t <$ logInfo (sformat ("[Time lord] System start: " %timestampF) t)

runSupporterReal :: BaseParams -> IO ()
runSupporterReal bp = runRealMode' bp return [] $ do
    supporterKey <- currentNodeKey
    logInfo $ sformat ("Supporter key: " % build) supporterKey
    repeatForever (sec 5) (const . return $ sec 5) $ do
      getKnownPeers >>= logInfo . sformat ("Known peers: " % build)

-- | Run full node in real mode.
runNodeReal :: NodeParams -> IO ()
runNodeReal np@(NodeParams {..}) = runRealMode np listeners runNode
  where
    listeners = if isDevelopment
                   then sysStartReqListener (Just npSystemStart) : allListeners
                   else allListeners

----------------------------------------------------------------------------
-- WorkMode implementations
----------------------------------------------------------------------------

type BenchRunner m a = m RealWithoutNetwork a -> RealWithoutNetwork a

runSemiRealMode :: (WorkIOMode (m RealWithoutNetwork), MonadDialog (m RealWithoutNetwork))
                => BenchRunner m a
                -> NodeParams -> [ListenerDHT (SemiRealMode m)]
                -> SemiRealMode m a
                -> IO a
runSemiRealMode runBench p@(NodeParams {..}) listeners action = runRealMode' npBaseParams modifier listeners action
  where
    modifier m = do
      db <- openDb p
      return . runDH db . runCH p . runBench $ m

runDH :: NodeState -> DBHolder m a -> m a
runDH db = flip runReaderT db . getDBHolder

runCH :: NodeParams -> ContextHolder m a -> m a
runCH NodeParams {..} = flip runReaderT ctx . getContextHolder
  where
    ctx =
        NodeContext
        { ncSystemStart = npSystemStart
        , ncSecretKey = npSecretKey
        , ncVssKeyPair = npVssKeyPair
        }

openDb :: NodeParams -> IO NodeState
openDb p@(NodeParams {..}) = runTimed (lpRootLogger . bpLogging $ npBaseParams) . runCH p $
  maybe openMemState (openState npRebuildDb) npDbPath


-- TODO: use bracket
runRealMode' :: (WithNamedLogger m, MonadIO m, MonadTimed m, MonadMask m, MonadDialog m) => BaseParams -> (m a -> IO (BinaryDialog Transfer a)) -> [ListenerDHT (KademliaDHT m)] -> KademliaDHT m a -> IO a
runRealMode' BaseParams {..} mModifier listeners action = do
    setupLoggingReal bpLogging
    runTimed (lpRootLogger bpLogging)
      =<< mModifier ( runKademliaDHT kadConfig (logInfo onStartMsg >> action) )
  where
    onStartMsg = sformat ("Started node, joining to DHT network "%build) bpDHTPeers
    kadConfig =
      KademliaDHTConfig
      { kdcKeyOrType = bpDHTKeyOrType
      , kdcPort = bpPort
      , kdcListeners = listeners
      , kdcMessageCacheSize = 1000000
      , kdcEnableBroadcast = False
      , kdcInitialPeers = bpDHTPeers
      , kdcNoCacheMessageNames = if isDevelopment
                                    then sysStartMessageNames
                                    else []
      }

runTimed :: LoggerName -> BinaryDialog Transfer a -> IO a
runTimed loggerName =
    runTimedIO .
    usingLoggerName loggerName . runTransfer . runBinaryDialog

runRealMode :: NodeParams -> [ListenerDHT RealMode] -> RealMode a -> IO a
runRealMode = runSemiRealMode runNoBenchmarksT

runBenchMode :: NodeParams -> [ListenerDHT BenchMode] -> BenchMode a -> IO a
runBenchMode = runSemiRealMode runBenchmarkT

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
