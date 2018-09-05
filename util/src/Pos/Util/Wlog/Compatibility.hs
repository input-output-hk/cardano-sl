{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

module Pos.Util.Wlog.Compatibility
        (  -- * CanLog
           CanLog (..)
         , WithLogger
           -- * Pure logging
         , dispatchEvents
         , LogEvent (..)
         , setupLogging
           -- * Logging functions
         , logDebug
         , logError
         , logInfo
         , logNotice
         , logWarning
         , logMessage
         , LoggerName
         , LoggerNameBox (..)
         , HasLoggerName (..)
         , usingLoggerName
         , Severity (..)
           -- * LoggerConfig
         , productionB
         , retrieveLogContent
           -- * Safe logging
         , SelectionMode
         , logMCond
           -- * Named Pure logging
         , NamedPureLogger (..)
         , launchNamedPureLog
         , runNamedPureLog
           -- * reimplementations
         , removeAllHandlers
         , centiUtcTimeF
         , getLinesLogged
         ) where

import           Control.Concurrent (isEmptyMVar, myThreadId)
import           Control.Lens (each)
import           Control.Monad.Base (MonadBase)
import           Control.Monad.Morph (MFunctor (..))
import qualified Control.Monad.State.Lazy as StateLazy
import           Control.Monad.Trans.Control (MonadBaseControl (..))
import           Data.Map.Strict (lookup)
import           Data.Sequence ((|>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Time (UTCTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Language.Haskell.TH as TH

import           Pos.Util.Log (LoggingHandler, Severity (..))
import qualified Pos.Util.Log as Log
import qualified Pos.Util.Log.Internal as Internal
import           Pos.Util.Log.LoggerConfig (LogHandler (..),
                     LogSecurityLevel (..), LoggerConfig (..),
                     defaultInteractiveConfiguration, lcLoggerTree, lhName,
                     ltHandlers)
import           System.IO.Unsafe (unsafePerformIO)

import           Universum

import qualified Katip as K
import qualified Katip.Core as KC

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
        mayEnv <- Internal.getLogEnv lh
        case mayEnv of
            Nothing -> error "logging not yet initialized. Abort."
            Just env -> Log.logItem' ()
                                     (K.Namespace [name])
                                     env
                                     Nothing
                                     (Internal.sev2klog severity)
                                     (K.logStr msg)

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
loggingHandler = unsafePerformIO $ do newEmptyMVar

-- | setup logging according to configuration @LoggerConfig@
--   the backends (scribes) will be registered with katip
setupLogging :: MonadIO m => LoggerConfig -> m ()
setupLogging lc = do
    nologging <- liftIO $ isEmptyMVar loggingHandler
    --unless nologging $ error "logging already setup!"
    when nologging
        $ do { lh <- Log.setupLogging lc
             ; putMVar loggingHandler lh }

-- LogSafe

-- | Whether to log to given log handler.
type SelectionMode = LogSecurityLevel -> Bool

-- | this emulates katip's 'logItem' function, but only outputs the message
--   to scribes which match the 'SelectionMode'
logItemS
    :: (K.LogItem a, MonadIO m)
    => LoggingHandler
    -> a
    -> K.Namespace
    -> Maybe TH.Loc
    -> K.Severity
    -> SelectionMode
    -> K.LogStr
    -> m ()
logItemS lhandler a ns loc sev cond msg = do
    mayle <- liftIO $ Internal.getLogEnv lhandler
    case mayle of
        Nothing -> error "logging not yet initialized. Abort."
        Just le -> do
            maycfg <- liftIO $ Internal.getConfig lhandler
            let cfg = case maycfg of
                    Nothing -> error "No Configuration for logging found. Abort."
                    Just c  -> c
            liftIO $ do
                item <- K.Item
                    <$> pure (K._logEnvApp le)
                    <*> pure (K._logEnvEnv le)
                    <*> pure sev
                    <*> (KC.mkThreadIdText <$> myThreadId)
                    <*> pure (K._logEnvHost le)
                    <*> pure (K._logEnvPid le)
                    <*> pure a
                    <*> pure msg
                    <*> (K._logEnvTimer le)
                    <*> pure ((K._logEnvApp le) <> ns)
                    <*> pure loc
                let lhs = cfg ^. lcLoggerTree ^. ltHandlers ^.. each
                forM_ (filterWithSafety cond lhs) (\ lh -> do
                    case lookup (lh ^. lhName) (K._logEnvScribes le) of
                        Nothing -> error ("Not found Scribe with name: " <> lh ^. lhName)
                        Just scribeH -> atomically
                            (KC.tryWriteTBQueue (KC.shChan scribeH) (KC.NewItem item)))
            where
              filterWithSafety :: SelectionMode -> [LogHandler] -> [LogHandler]
              filterWithSafety condition = filter (\lh -> case _lhSecurityLevel lh of
                  Nothing -> False
                  Just s  -> condition s)

logMCond :: MonadIO m => LoggerName -> Severity -> Text -> SelectionMode -> m ()
logMCond name severity msg cond = do
    let ns = K.Namespace [name]
    lh <- liftIO $ readMVar loggingHandler
    logItemS lh () ns Nothing (Internal.sev2klog severity) cond $ K.logStr msg

getLinesLogged :: IO Integer
getLinesLogged = do
    lh <- liftIO $ readMVar loggingHandler
    Internal.getLinesLogged lh

-- | various reimplementations

productionB :: LoggerConfig
productionB = defaultInteractiveConfiguration Debug

retrieveLogContent :: FilePath -> Maybe Int -> IO [Text]
retrieveLogContent fp maylines = do
    let nlines = fromMaybe 9999 maylines
    ((take nlines) . reverse . lines) <$> TIO.readFile fp

centiUtcTimeF :: UTCTime -> Text
centiUtcTimeF utc =
    T.pack $ formatTime defaultTimeLocale tsformat utc
  where
    tsformat :: String
    tsformat = "%F %T%2Q %Z"

-- do nothing, logs are closed by finalizers
removeAllHandlers :: IO ()
removeAllHandlers = pure ()

