{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

module Pos.Util.Klog.Compatibility
        (  -- * CanLog
           CanLog (..)
         , WithLogger
           -- * Pure logging
         , dispatchEvents
         , LogEvent (..)
         , setupLogging
         , setupLogging'
         , setupTestLogging
           -- * Logging functions
         , logDebug
         , logError
         , logInfo
         , logNotice
         , logWarning
         , logMessage
         , LoggerNameBox (..)
         , HasLoggerName (..)
         , usingLoggerName
         , addLoggerName
         , Severity (..)
         , LoggerConfig
           -- * LoggerConfig
         , productionB
         , retrieveLogContent
           -- * Named Pure logging
         , NamedPureLogger (..)
         , launchNamedPureLog
         , runNamedPureLog
         , removeAllHandlers
         , usingNamedPureLogger
         , LoggingHandler (..)
         , LoggingHandlerInternal (..)
           -- * Logging configuration
         , defaultTestConfiguration
         , defaultInteractiveConfiguration
         , parseLoggerConfig
         , setLogPrefix
         , injectTrace
         , emptyConf
         , setupFromRepresentation
         , getTrace
         ) where

import           Control.Concurrent (modifyMVar_)
import           Control.Concurrent.MVar (withMVar)
import           Control.Monad.Base (MonadBase)
import           Control.Monad.Morph (MFunctor (..))
import qualified Control.Monad.State.Lazy as StateLazy
import           Control.Monad.Trans.Control (MonadBaseControl (..))
import           Data.Sequence ((|>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import           Cardano.BM.Configuration (Configuration)
import qualified Cardano.BM.Configuration.Model as Configuration
import           Cardano.BM.Configuration.Static (defaultConfigStdout,
                     defaultConfigTesting)
import           Cardano.BM.Data.Configuration (Representation (..))
import           Cardano.BM.Data.LogItem (LogSelection (Both))
import           Cardano.BM.Data.Severity (Severity (..))
import           Cardano.BM.Data.Trace (Trace)
import           Cardano.BM.Setup (setupTrace)
import           Cardano.BM.Trace (appendName, traceNamedItem)
import qualified Pos.Util.Log as Log
import           System.IO.Unsafe (unsafePerformIO)

import           Universum

-- set log prefix. (Logs prefix is no more used)
setLogPrefix :: Maybe FilePath -> LoggerConfig -> IO LoggerConfig
setLogPrefix _ lc = return lc

parseLoggerConfig :: MonadIO m => FilePath -> m LoggerConfig
parseLoggerConfig path = liftIO $ Configuration.setup path

defaultTestConfiguration :: Severity -> LoggerConfig
defaultTestConfiguration _ = unsafePerformIO defaultConfigTesting

defaultInteractiveConfiguration :: Severity -> LoggerConfig
defaultInteractiveConfiguration _ = unsafePerformIO defaultConfigStdout

type LoggerConfig = Configuration

data LoggingHandlerInternal = LoggingHandlerInternal
    { lhiTrace :: Trace IO
    }

-- | internal data structure to be passed around
type LoggingMVar = MVar LoggingHandlerInternal
newtype LoggingHandler = LoggingHandler
    { getLSI :: LoggingMVar
    }

-- | inject an external Trace into logging handler in order to enable
-- logging
injectTrace :: Trace IO -> IO ()
injectTrace tr = do
    lhiMVar <- newMVar $ LoggingHandlerInternal tr
    modifyMVar_ loggingHandler $ const $ return $ LoggingHandler lhiMVar

getTrace :: LoggingHandler -> IO (Trace IO)
getTrace lh = withMVar (getLSI lh) $ \lhi -> return $ lhiTrace lhi

type LoggerName = Log.LoggerName

class Monad m => CanLog m where
    dispatchMessage :: LoggerName -> Severity -> Text -> m ()

    default dispatchMessage :: (MonadTrans t, t n ~ m, CanLog n)
                            => LoggerName
                            -> Severity
                            -> Text
                            -> m ()
    dispatchMessage name severity msg = lift $ dispatchMessage name severity msg

instance CanLog m => CanLog (LoggerNameBox m)
instance CanLog m => CanLog (ReaderT r m)
instance CanLog m => CanLog (StateT s m)
instance CanLog m => CanLog (StateLazy.StateT s m)
instance CanLog m => CanLog (ExceptT s m)

instance CanLog IO where
    dispatchMessage name severity msg = do
        lh <- readMVar loggingHandler
        tr <- getTrace lh
        tr' <- appendName name tr
        traceNamedItem tr' Both severity msg

type WithLogger m = (CanLog m, HasLoggerName m)

logDebug, logInfo, logNotice, logWarning, logError
  :: WithLogger m
  => Text -> m ()
logDebug   = logMessage Debug
logInfo    = logMessage Info
logNotice  = logMessage Notice
logWarning = logMessage Warning
logError   = logMessage Error

logMessage :: WithLogger m => Severity -> Text -> m ()
logMessage severity msg = do
    name <- askLoggerName
    dispatchMessage name severity msg

newtype LoggerNameBox m a = LoggerNameBox
    { loggerNameBoxEntry :: ReaderT LoggerName m a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadTrans,
                MonadBase b, MonadThrow, MonadCatch, MonadMask, MonadState s)

instance MonadReader r m => MonadReader r (LoggerNameBox m) where
    ask = lift ask
    reader = lift . reader
    local f (LoggerNameBox m) = askLoggerName >>= lift . local f . runReaderT m

instance MonadBaseControl b m => MonadBaseControl b (LoggerNameBox m) where
    type StM (LoggerNameBox m) a = StM (ReaderT LoggerName m) a
    liftBaseWith io =
        LoggerNameBox $ liftBaseWith $ \runInBase -> io $ runInBase . loggerNameBoxEntry
    restoreM = LoggerNameBox . restoreM

instance MFunctor LoggerNameBox where
    hoist f = LoggerNameBox . hoist f . loggerNameBoxEntry

usingLoggerName :: LoggerName -> LoggerNameBox m a -> m a
usingLoggerName name = flip runReaderT name . loggerNameBoxEntry

class HasLoggerName m where

  askLoggerName :: m LoggerName
  modifyLoggerName :: (LoggerName -> LoggerName) -> m a -> m a

  default askLoggerName :: (MonadTrans t, t n ~ m, Monad n, HasLoggerName n)
                        => m LoggerName
  askLoggerName = lift askLoggerName

  default modifyLoggerName :: (MFunctor t, t n ~ m, Monad n, HasLoggerName n)
                           => (LoggerName -> LoggerName) -> m a -> m a
  modifyLoggerName f = hoist (modifyLoggerName f)

instance HasLoggerName IO where
    askLoggerName    = pure "*production*"
    modifyLoggerName = const id

instance (Monad m, HasLoggerName m) => HasLoggerName (StateT a m)
instance (Monad m, HasLoggerName m) => HasLoggerName (StateLazy.StateT a m)
instance (Monad m, HasLoggerName m) => HasLoggerName (ReaderT a m)
instance HasLoggerName Identity where
    askLoggerName    = Identity "Identity"
    modifyLoggerName = flip const

instance (Monad m, HasLoggerName m) => HasLoggerName (ExceptT e m)
instance Monad m => HasLoggerName (LoggerNameBox m) where
  askLoggerName = LoggerNameBox ask
  modifyLoggerName how = LoggerNameBox . local how . loggerNameBoxEntry

addLoggerName :: HasLoggerName m => LoggerName -> m a -> m a
addLoggerName m = modifyLoggerName (<> ("." `T.append` m))

-- setLoggerName :: HasLoggerName m => LoggerName -> m a -> m a
-- setLoggerName m = modifyLoggerName (const m)

launchNamedPureLog
    :: (WithLogger n, Monad m)
    => (forall f. Functor f => m (f a) -> n (f b))
    -> NamedPureLogger m a
    -> n b
launchNamedPureLog hoist' namedPureLogger = do
    name <- askLoggerName
    (logs, res) <- hoist' $ swap <$> usingNamedPureLogger name namedPureLogger
    res <$ dispatchEvents logs

usingNamedPureLogger :: Functor m
                     => LoggerName
                     -> NamedPureLogger m a
                     -> m (a, [LogEvent])
usingNamedPureLogger name (NamedPureLogger action) =
    usingLoggerName name $ runPureLog action

data LogEvent = LogEvent
    { leLoggerName :: !LoggerName
    , leSeverity   :: !Severity
    , leMessage    :: !Text
    } deriving (Show)

runPureLog :: Functor m => PureLogger m a -> m (a, [LogEvent])
runPureLog = fmap (second toList) . usingStateT mempty . runPureLogger

newtype PureLogger m a = PureLogger
    { runPureLogger :: StateT (Seq LogEvent) m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadState (Seq LogEvent),
                MonadThrow, HasLoggerName)

instance Monad m => CanLog (PureLogger m) where
    dispatchMessage name severity message = modify' (|> (LogEvent name severity message))
instance MFunctor PureLogger where
    hoist f = PureLogger . hoist f . runPureLogger


newtype NamedPureLogger m a = NamedPureLogger
    { runNamedPureLogger :: PureLogger (LoggerNameBox m) a
    } deriving (Functor, Applicative, Monad, MonadState (Seq LogEvent),
                MonadThrow, HasLoggerName)

instance MonadTrans NamedPureLogger where
    lift = NamedPureLogger . lift . lift
instance Monad m => CanLog (NamedPureLogger m) where
    dispatchMessage name sev msg =
        NamedPureLogger $ dispatchMessage name sev msg
instance MFunctor NamedPureLogger where
    hoist f = NamedPureLogger . hoist (hoist f) . runNamedPureLogger
instance MonadIO m => MonadIO (NamedPureLogger m) where
    liftIO = lift . liftIO

runNamedPureLog
    :: (Monad m, HasLoggerName m)
    => NamedPureLogger m a -> m (a, [LogEvent])
runNamedPureLog (NamedPureLogger action) =
    askLoggerName >>= (`usingLoggerName` runPureLog action)

dispatchEvents :: CanLog m => [LogEvent] -> m ()
dispatchEvents = mapM_ dispatchLogEvent
  where
    dispatchLogEvent (LogEvent name sev t) = dispatchMessage name sev t

-- | internal access to logging handler
{-# NOINLINE loggingHandler #-}
loggingHandler :: MVar LoggingHandler
loggingHandler = unsafePerformIO $ do
    newMVar $ error "LoggingHandler MVar is not initialized."

-- | setup logging used in tests
setupTestLogging :: IO ()
setupTestLogging = setupLogging "test" (defaultTestConfiguration Debug)

-- | setup logging according to configuration @LoggerConfig@
--   the backends (scribes) will be registered with katip
setupLogging :: MonadIO m => Text -> LoggerConfig -> m ()
setupLogging cfoKey lc = liftIO $ do
    tr <- setupTrace (Right lc) cfoKey
    lhiMVar <- newMVar $ LoggingHandlerInternal tr
    modifyMVar_ loggingHandler $ const $ return $ LoggingHandler lhiMVar

--  | Same with 'setupLogging' but also returns the 'LoggingHandler'
setupLogging' :: MonadIO m => Text -> LoggerConfig -> m LoggingHandler
setupLogging' cfoKey lc = liftIO $ do
    setupLogging cfoKey lc
    readMVar loggingHandler

-- | various reimplementations

productionB :: LoggerConfig
productionB = defaultInteractiveConfiguration Debug

retrieveLogContent :: FilePath -> Maybe Int -> IO [Text]
retrieveLogContent fp maylines = do
    let nlines = fromMaybe 9999 maylines
    ((take nlines) . reverse . lines) <$> TIO.readFile fp

-- do nothing, logs are closed by finalizers
removeAllHandlers :: LoggingHandler -> IO ()
removeAllHandlers _ = pure () --unrealize

emptyConf :: Configuration
emptyConf = unsafePerformIO Configuration.empty

setupFromRepresentation :: Representation -> Configuration
setupFromRepresentation = unsafePerformIO . Configuration.setupFromRepresentation
