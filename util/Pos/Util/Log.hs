-- | Logging implemented with library `katip`
module Pos.Util.Log
       (
       -- * Logging
         Severity(..)
       , LogContext
       , LogContextT
       , LoggingHandler
       -- * Compatibility
       , CanLog(..)
       , HasLoggerName(..)
       , WithLogger
       , LoggerNameBox(..)
       -- * Configuration
       , LoggerConfig(..)
       , loadLogConfig
       , parseLoggerConfig
       , retrieveLogFiles
       , setLogBasePath
       -- * Startup
       , setupLogging
       -- * Do logging
       , loggerBracket
       , usingLoggerName
       -- * Functions
       , logDebug
       , logInfo
       , logNotice
       , logWarning
       , logError
       , logMessage
       -- * Naming/Context
       , LoggerName
       , askLoggerName
       , addLoggerName
       ) where

import           Universum

import           Control.Lens (each)
import           Control.Monad.Base (MonadBase)
import           Control.Monad.Morph (MFunctor (..))
import           Control.Monad.Writer (WriterT (..))

import           Pos.Util.Log.Severity (Severity (..))
import           Pos.Util.LoggerConfig

import qualified Data.Text as T
import           Data.Text.Lazy.Builder

import           Pos.Util.Log.Internal (LoggingHandler)
import qualified Pos.Util.Log.Internal as Internal
import           Pos.Util.Log.Scribes

import qualified Katip as K
import qualified Katip.Core as KC


-- | alias - pretend not to depend on katip
type LogContext = K.KatipContext
type LogContextT = K.KatipContextT

type WithLogger m = (CanLog m, HasLoggerName m)

type LoggerName = Text

-- -- | compatibility
class (MonadIO m, LogContext m) => CanLog m where
    dispatchMessage :: LoggingHandler -> Severity -> Text -> m ()
    dispatchMessage _ s t = K.logItemM Nothing (Internal.sev2klog s) $ K.logStr t

class (MonadIO m, LogContext m) => HasLoggerName m where
    askLoggerName' :: m LoggerName
    askLoggerName' = askLoggerName
    setLoggerName' :: LoggerName -> m a -> m a
    setLoggerName' = modifyLoggerName' . const
    modifyLoggerName' :: (LoggerName -> LoggerName) -> m a -> m a
    modifyLoggerName' f a = addLoggerName (f "cardano-sl")$ a
instance (Monad m, HasLoggerName m) => HasLoggerName (ReaderT a m) where
instance (Monad m, HasLoggerName m) => HasLoggerName (StateT a m) where
instance (Monoid w, Monad m, HasLoggerName m) => HasLoggerName (WriterT w m) where
instance (Monad m, HasLoggerName m) => HasLoggerName (ExceptT e m) where
    askLoggerName'    = lift askLoggerName'

newtype LoggerNameBox m a = LoggerNameBox
    { loggerNameBoxEntry :: ReaderT LoggerName m a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadBase b, MonadState s)
instance MFunctor LoggerNameBox where
    hoist f = LoggerNameBox . hoist f . loggerNameBoxEntry

instance CanLog (LogContextT IO)
instance CanLog m => CanLog (ReaderT s m)
instance CanLog m => CanLog (StateT s m)
instance CanLog m => CanLog (ExceptT s m)

instance HasLoggerName (LogContextT IO)


-- | log a Text with severity
logMessage :: (LogContext m {-, HasCallStack -}) => Severity -> Text -> m ()
logMessage sev msg = logMessage' (Internal.sev2klog sev) $ K.logStr msg
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
askLoggerName :: LogContext m => m LoggerName
askLoggerName = do
    ns <- K.getKatipNamespace
    return $ toStrict $ toLazyText $ mconcat $ map fromText $ KC.intercalateNs ns

-- | push a local name
addLoggerName :: LogContext m => LoggerName -> m a -> m a
addLoggerName t f =
    K.katipAddNamespace (KC.Namespace [t]) $ f

-- | setup logging according to configuration @LoggerConfig@
--   the backends (scribes) will be registered with katip
setupLogging :: LoggerConfig -> IO LoggingHandler
setupLogging lc = do
    lh <- Internal.newConfig lc
    scribes <- meta lh lc
    Internal.registerBackends lh scribes
    return lh
      where
        meta :: LoggingHandler -> LoggerConfig -> IO [(T.Text, K.Scribe)]
        meta _lh _lc = do
            -- setup scribes according to configuration
            let --minSev = _lc ^. lcLoggerTree ^. ltMinSeverity
                lhs = _lc ^. lcLoggerTree ^. ltHandlers ^.. each
                basepath = _lc ^. lcBasePath
            forM lhs (\lh -> case (lh ^. lhBackend) of
                    FileJsonBE -> do
                        putStrLn ("creating JSON backend ..." :: Text)
                        scribe <- mkJsonFileScribe
                                      (fromMaybe "." basepath)
                                      (fromMaybe "<unk>" $ lh ^. lhFpath)
                                      (Internal.sev2klog $ fromMaybe Debug $ lh ^. lhMinSeverity)
                                      K.V0
                        return (lh ^. lhName, scribe)
                    FileTextBE -> do
                        scribe <- mkFileScribe
                                      (fromMaybe "." basepath)
                                      (fromMaybe "<unk>" $ lh ^. lhFpath)
                                      True
                                      (Internal.sev2klog $ fromMaybe Debug $ lh ^. lhMinSeverity)
                                      K.V0
                        return (lh ^. lhName, scribe)
                    StdoutBE -> do
                        scribe <- mkStdoutScribe
                                      (Internal.sev2klog $ fromMaybe Debug $ lh ^. lhMinSeverity)
                                      K.V0
                        return (lh ^. lhName, scribe)
                    DevNullBE -> do
                        scribe <- mkDevNullScribe _lh
                                      (Internal.sev2klog $ fromMaybe Debug $ lh ^. lhMinSeverity)
                                      K.V0
                        return (lh ^. lhName, scribe)
                 )


{-| provide logging in IO

* example

    @
      lh <- setupLogging logconf
      usingLoggerName lh "processXYZ" $
          logInfo "entering"
          complexWork "42"
          logInfo "done."

      where
          complexWork :: WithLogger m => String -> m ()
          complexWork m = do
              logDebug $ "let's see: " `append` m
    @
-}
usingLoggerName :: LoggingHandler -> LoggerName -> LogContextT IO a -> IO a
usingLoggerName lh name f = do
    mayle <- Internal.getLogEnv lh
    case mayle of
            Nothing -> error "logging not yet initialized. Abort."
            Just le -> K.runKatipContextT le () (Internal.s2kname name) $ f

{-| bracket logging

!! this will close the backends at the end of the action !!


* example

    @
      lh <- setupLogging logconf
      loggerBracket lh "processXYZ" $
          logInfo "entering"
          complexWork "42"
          logInfo "done."

      where
          complexWork :: WithLogger m => String -> m ()
          complexWork m =
              addLoggerName "in_complex" $ do
                  logDebug $ "let's see: " `append` m
    @

-}
loggerBracket :: LoggingHandler -> LoggerName -> LogContextT IO a -> IO a
loggerBracket lh name f = do
    mayle <- Internal.getLogEnv lh
    case mayle of
            Nothing -> error "logging not yet initialized. Abort."
            Just le -> bracket (return le) K.closeScribes $
                          \le_ -> K.runKatipContextT le_ () (Internal.s2kname name) $ f

-- | for compatibility (TODO check if still referenced)
loadLogConfig :: Maybe FilePath -> Maybe FilePath -> IO LoggingHandler
loadLogConfig pre cfg = do
    lc0 <- case cfg of
              Nothing -> return (mempty :: LoggerConfig)
              Just fp -> parseLoggerConfig fp
    setLogPrefix pre lc0 >>= setupLogging



-- | set base path of logging in @LoggerConfig@
setLogBasePath :: LoggingHandler -> FilePath -> IO ()
setLogBasePath lh fp = do
    maycfg <- Internal.getConfig lh
    case maycfg of
              Nothing  -> return ()
              Just cfg -> Internal.updateConfig lh cfg{ _lcBasePath = Just fp}


{- |
   * interactive tests

   >>> lh <- setupLogging $ defaultInteractiveConfiguration Info
   >>> loggerBracket lh "testtest" $ do { logInfo "This is a message" }

   >>> lh <- setupLogging $ defaultInteractiveConfiguration Info
   >>> loggerBracket lh "testtest" $ do { logDebug "You won't see this message" }

   >>> lh <- setupLogging $ defaultInteractiveConfiguration Info
   >>> loggerBracket lh "testtest" $ do { logWarning "Attention!"; addLoggerName "levelUp" $ do { logError "..now it happened" } }

   >>> lh <- setupLogging $ defaultInteractiveConfiguration Info
   >>> usingLoggerName lh "testmore" $ do { logInfo "hello..." }
-}

