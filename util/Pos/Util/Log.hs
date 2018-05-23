{-# LANGUAGE LambdaCase #-}

module Pos.Util.Log
       ( Severity(..)
       , LogContext
       , LogContextT
       ---
       , CanLog(..)
       , HasLoggerName(..)
       , WithLogger
       , LoggerNameBox(..)
       ---
       , LoggerConfig(..)
       , loadLogConfig
       , parseLoggerConfig
       , retrieveLogFiles
       ---
       , loggerBracket
       ---
       , logDebug
       , logInfo
       , logNotice
       , logWarning
       , logError
       , logMessage
       ---
       , LoggerName
       , addLoggerName
       , usingLoggerName
       ) where

import           Universum

import           Control.Monad.Base (MonadBase)
import           Control.Monad.Morph (MFunctor(..))
import           Control.Monad.Writer (WriterT (..))

import           Pos.Util.LoggerConfig (LoggerConfig(..), parseLoggerConfig, loadLogConfig, retrieveLogFiles)
import           Pos.Util.LogSeverity (Severity(..))
import           Pos.Util.LogStdoutScribe (mkStdoutScribe)

import           Data.Text (Text{-, unpack-})
import           Data.Text.Lazy.Builder

import qualified Katip                      as K
import qualified Katip.Core                 as KC


-- | alias - pretend not to depend on katip
type LogContext = K.KatipContext
type LogContextT = K.KatipContextT

type WithLogger m = (CanLog m, HasLoggerName m)

type LoggerName = Text

-- | compatibility
class (MonadIO m, LogContext m) => CanLog m where
    dispatchMessage :: LoggerName -> Severity -> Text -> m ()
    dispatchMessage _ s t = K.logItemM Nothing (sev2klog s) $ K.logStr t

class (MonadIO m, LogContext m) => HasLoggerName m where
    askLoggerName :: m LoggerName
    askLoggerName = askLoggerName0
    setLoggerName :: LoggerName -> m a -> m a
    setLoggerName = modifyLoggerName . const
    modifyLoggerName :: (LoggerName -> LoggerName) -> m a -> m a
    modifyLoggerName f a = addLoggerName (f "cardano-sl")$ a
instance (Monad m, HasLoggerName m) => HasLoggerName (ReaderT a m) where
instance (Monad m, HasLoggerName m) => HasLoggerName (StateT a m) where
instance (Monoid w, Monad m, HasLoggerName m) => HasLoggerName (WriterT w m) where
instance (Monad m, HasLoggerName m) => HasLoggerName (ExceptT e m) where
    askLoggerName    = lift askLoggerName

newtype LoggerNameBox m a = LoggerNameBox
    { loggerNameBoxEntry :: ReaderT LoggerName m a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadBase b, MonadState s)
instance MFunctor LoggerNameBox where
    hoist f = LoggerNameBox . hoist f . loggerNameBoxEntry
{-
instance WithLogger m => HasLoggerName (LoggerNameBox m) where
    askLoggerName = LoggerNameBox ask
    modifyLoggerName how = LoggerNameBox . local how . loggerNameBoxEntry
instance (MonadReader r m, CanLog m) => MonadReader r (LoggerNameBox m) where
    ask = lift ask
    reader = lift . reader
    local f (LoggerNameBox m) = askLoggerName >>= lift . local f . runReaderT m
-}
instance CanLog (LogContextT IO)
instance CanLog m => CanLog (ReaderT s m)
instance CanLog m => CanLog (StateT s m)
instance CanLog m => CanLog (ExceptT s m)

instance HasLoggerName (LogContextT IO)


-- | log a Text with severity
logMessage :: (LogContext m {-, HasCallStack -}) => Severity -> Text -> m ()
logMessage sev msg = logMessage' (sev2klog sev) $ K.logStr msg
logMessage' :: (LogContext m {-, HasCallStack -}) => K.Severity -> K.LogStr -> m ()
logMessage' s m = K.logItemM Nothing s m

-- | log a Text with severity = Debug
logDebug :: (LogContext m {-, HasCallStack -}) => Text -> m ()
logDebug msg = logMessage' K.DebugS $ K.logStr msg

-- | log a Text with severity = Info
logInfo :: (LogContext m {-, HasCallStack -}) => Text -> m ()
logInfo msg = K.logItemM Nothing K.InfoS $ K.logStr msg

-- | log a Text with severity = Notice
logNotice :: (LogContext m {-, HasCallStack -}) => Text -> m ()
logNotice msg = K.logItemM Nothing K.NoticeS $ K.logStr msg

-- | log a Text with severity = Warning
logWarning :: (LogContext m {-, HasCallStack -}) => Text -> m ()
logWarning msg = K.logItemM Nothing K.WarningS $ K.logStr msg

-- | log a Text with severity = Error
logError :: (LogContext m {-, HasCallStack -}) => Text -> m ()
logError msg = K.logItemM Nothing K.ErrorS $ K.logStr msg


-- | get current stack of logger names
askLoggerName0 :: (MonadIO m, LogContext m) => m LoggerName
askLoggerName0 = do
    ns <- K.getKatipNamespace
    return $ toStrict $ toLazyText $ mconcat $ map fromText $ KC.intercalateNs ns

-- | push a local name
addLoggerName :: (MonadIO m, LogContext m) => LoggerName -> m a -> m a
addLoggerName t f =
    K.katipAddNamespace (KC.Namespace [t]) $ f

-- | WIP -- do not use
-- type NamedPureLogger m a = LogContextT m a
{-
newtype NamedPureLogger m a = NamedPureLogger
    { runNamedPureLogger :: LogContextT m a }
    deriving (Functor, Applicative, Monad,
              MonadThrow, LogContext)
-}
--instance (MonadIO m) => KC.Katip (NamedPureLogger m)

-- | translate Severity to Katip.Severity
sev2klog :: Severity -> K.Severity
sev2klog = \case
    Debug   -> K.DebugS
    Info    -> K.InfoS
    Notice  -> K.NoticeS
    Warning -> K.WarningS
    Error   -> K.ErrorS

-- | translate
s2kname :: Text -> K.Namespace
s2kname s = K.Namespace [s]

-- | setup logging
setupLogging :: Severity -> Text -> IO K.LogEnv
setupLogging minSev name = do
    --hScribe <- K.mkHandleScribe K.ColorIfTerminal stdout (sev2klog minSev) K.V0
    hScribe <- mkStdoutScribe (sev2klog minSev) K.V0
    le <- K.registerScribe "stdout" hScribe K.defaultScribeSettings =<< K.initLogEnv (s2kname name) "production"
    -- remember this K.LogEnv
    return le

-- | provide logging in IO
usingLoggerName :: Severity -> LoggerName -> LogContextT IO a -> IO a
usingLoggerName minSev name f = do
    le <- setupLogging minSev name
    K.runKatipContextT le () "cardano-sl" $ f

-- | bracket logging
loggerBracket :: Severity -> LoggerName -> LogContextT IO a -> IO a
loggerBracket minSev name f = do
    bracket (setupLogging minSev name) K.closeScribes $
      \le -> K.runKatipContextT le () "cardano-sl" $ f


-- | WIP: tests to run interactively in GHCi
--
{-
test1 :: IO ()
test1 = do
    loggerBracket Info "testtest" $ do
        logInfo "This is a message"

test2 :: IO ()
test2 = do
    loggerBracket Info "testtest" $ do
        logDebug "This is a DEBUG message"

test3 :: IO ()
test3 = do
    loggerBracket Info "testtest" $ do
        logWarning "This is a warning!"
        addLoggerName "onTop" $ do
            ns <- askLoggerName
            logWarning "This is a last warning!"
            putStrLn $ "loggerName = " ++ (unpack ns)

test4 :: IO ()
test4 = do
    usingLoggerName Info "testtest" $ do
        logWarning "This is a warning!"
-}

