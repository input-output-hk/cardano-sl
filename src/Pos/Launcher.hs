{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Launcher of full node or simple operations.

module Pos.Launcher
       ( LoggingParams (..)
       , NodeParams (..)
       , SupporterParams (..)
       , getCurTimestamp
       , runNode
       , runNodeReal
       , submitTx
       , submitTxReal
       , runSupporterReal
       ) where

import           Control.TimeWarp.Logging (LoggerName, Severity (Warning), initLogging,
                                           logError, logInfo, setSeverity,
                                           setSeverityMaybe, usingLoggerName)
import           Control.TimeWarp.Rpc     (BinaryDialog, NetworkAddress, Transfer,
                                           runBinaryDialog, runTransfer)
import           Control.TimeWarp.Timed   (currentTime, repeatForever, runTimedIO, sec,
                                           sleepForever)
import           Data.Default             (Default (def))
import           Formatting               (build, sformat, (%))
import           Universum                hiding (catch)

import           Pos.Communication        (allListeners, sendTx)
import           Pos.Crypto               (SecretKey, VssKeyPair, hash, sign)
import           Pos.DHT                  (DHTKey, DHTNode (dhtAddr), DHTNodeType (..),
                                           ListenerDHT, MonadDHT (..), filterByNodeType)
import           Pos.DHT.Real             (KademliaDHTConfig (..), runKademliaDHT)
import           Pos.Slotting             (Timestamp (Timestamp), timestampF)
import           Pos.State                (NodeState, openMemState, openState)
import           Pos.Types                (Address, Coin, Tx (..), TxId, TxIn (..),
                                           TxOut (..), txF)
import           Pos.Worker               (runWorkers)
import           Pos.WorkMode             (ContextHolder (..), DBHolder (..),
                                           NodeContext (..), RealMode, WorkMode,
                                           getNodeContext, ncSecretKey)

-- | Get current time as Timestamp. It is intended to be used when you
-- launch the first node. It doesn't make sense in emulation mode.
getCurTimestamp :: IO Timestamp
getCurTimestamp = Timestamp <$> runTimedIO currentTime

-- | Run full node in any WorkMode.
runNode :: WorkMode m => NodeParams -> m ()
runNode NodeParams {..} = do
    peers <- discoverPeers DHTFull
    logInfo $ sformat ("Known peers: " % build) peers

    runWorkers
    sleepForever

data SupporterParams = SupporterParams
    { spLogging      :: !LoggingParams
    , spPort         :: !Word16
    , spDHTPeers     :: ![DHTNode]
    , spDHTKeyOrType :: Either DHTKey DHTNodeType
    } deriving (Show)

runSupporterReal :: SupporterParams -> IO ()
runSupporterReal SupporterParams {..} = do
    setupLoggingReal spLogging
    runTimed . runKademliaDHT supporterKadConfig $ main'
  where
    runTimed = runTimedIO . usingLoggerName (lpRootLogger spLogging) . runTransfer . runBinaryDialog
    main' = do
        supporterKey <- currentNodeKey
        logInfo $ sformat ("Supporter key: " % build) supporterKey
        repeatForever (sec 5) (const . return $ sec 5) $ do
          getKnownPeers >>= logInfo . sformat ("Known peers: " % build)
    supporterKadConfig = KademliaDHTConfig
                  { kdcKeyOrType = spDHTKeyOrType
                  , kdcPort = spPort
                  , kdcListeners = []
                  , kdcMessageCacheSize = 1000000
                  , kdcEnableBroadcast = False
                  , kdcInitialPeers = spDHTPeers
                  }

-- | Run full node in real mode.
runNodeReal :: NodeParams -> IO ()
runNodeReal p = runRealMode p allListeners $ runNode p

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
submitTxReal p input addrCoin = runRealMode p [] $
    (fmap dhtAddr . filterByNodeType DHTFull) <$> getKnownPeers >>= \na -> submitTx na input addrCoin

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
    { npDbPath       :: !(Maybe FilePath)
    , npRebuildDb    :: !Bool
    , npSystemStart  :: !(Maybe Timestamp)
    , npLogging      :: !LoggingParams
    , npSecretKey    :: !SecretKey
    , npVssKeyPair   :: !VssKeyPair
    , npPort         :: !Word16
    , npDHTPeers     :: ![DHTNode]
    , npDHTKeyOrType :: Either DHTKey DHTNodeType
    } deriving (Show)

----------------------------------------------------------------------------
-- WorkMode implementations
----------------------------------------------------------------------------

-- TODO: use bracket
runRealMode :: NodeParams -> [ListenerDHT RealMode] -> RealMode a -> IO a
runRealMode NodeParams {..} listeners action = do
    setupLoggingReal npLogging
    startTime <- getStartTime
    db <- (runTimed . runCH startTime) openDb
    let onStartMsg =
            sformat ("Started node, joining to DHT network "%build) npDHTPeers
    runTimed . runDH db . runCH startTime . runKademliaDHT kadConfig $
        do logInfo onStartMsg
           action
  where
    kadConfig =
      KademliaDHTConfig
      { kdcKeyOrType = npDHTKeyOrType
      , kdcPort = npPort
      , kdcListeners = listeners
      , kdcMessageCacheSize = 1000000
      , kdcEnableBroadcast = False
      , kdcInitialPeers = npDHTPeers
      }
    getStartTime =
        case npSystemStart of
            Just t -> pure t
            Nothing ->
                runTimed $
                do t <- Timestamp <$> currentTime
                   t <$ putText (sformat ("System start: " %timestampF) t)
    openDb = maybe openMemState (openState npRebuildDb) npDbPath
    ctx startTime =
        NodeContext
        { ncSystemStart = startTime
        , ncSecretKey = npSecretKey
        , ncVssKeyPair = npVssKeyPair
        }
    runCH :: Timestamp -> ContextHolder m a -> m a
    runCH startTime = flip runReaderT (ctx startTime) . getContextHolder
    runTimed :: BinaryDialog Transfer a -> IO a
    runTimed =
        runTimedIO .
        usingLoggerName (lpRootLogger npLogging) . runTransfer . runBinaryDialog
    runDH :: NodeState -> DBHolder m a -> m a
    runDH db = flip runReaderT db . getDBHolder

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
