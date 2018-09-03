-- | Logging implemented with library `katip`

module Pos.Util.Log
       (
       -- * Logging
         Severity (..)
       , LogContext
       , LogContextT
       , LoggingHandler
       -- * Compatibility
       , CanLog (..)
       , WithLogger
       -- * Configuration
       , LoggerConfig (..)
       , parseLoggerConfig
       , retrieveLogFiles
       -- * Startup
       , setupLogging
       -- * Do logging
       , loggerBracket
       , usingLoggerName
       , usingLoggerNames
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
       -- * other functions
       , logItem'
       ) where

import           Universum

import           Control.Concurrent (myThreadId)
import           Control.Lens (each)
import qualified Data.Text as T
import           Data.Text.Lazy.Builder
import qualified Language.Haskell.TH as TH

import           Pos.Util.Log.Internal (LoggingHandler)
import qualified Pos.Util.Log.Internal as Internal
import           Pos.Util.Log.LoggerConfig
import           Pos.Util.Log.LoggerName (LoggerName)
import           Pos.Util.Log.Scribes
import           Pos.Util.Log.Severity (Severity (..))

import qualified Katip as K
import qualified Katip.Core as KC


-- | alias - pretend not to depend on katip
type LogContext = K.KatipContext
type LogContextT = K.KatipContextT

type WithLogger m = (CanLog m)

-- -- | compatibility
class (MonadIO m, LogContext m) => CanLog m where
    dispatchMessage :: LoggingHandler -> Severity -> Text -> m ()
    dispatchMessage _ s t = K.logItemM Nothing (Internal.sev2klog s) $ K.logStr t

instance CanLog (LogContextT IO)
instance CanLog m => CanLog (ReaderT s m)
instance CanLog m => CanLog (StateT s m)
instance CanLog m => CanLog (ExceptT s m)

-- | log a Text with severity
logMessage :: (LogContext m) => Severity -> Text -> m ()
logMessage sev msg = logMessage' (Internal.sev2klog sev) $ K.logStr msg
logMessage' :: (LogContext m) => K.Severity -> K.LogStr -> m ()
logMessage' s m = K.logItemM Nothing s m

-- | log a Text with severity = Debug
logDebug :: (LogContext m) => Text -> m ()
logDebug msg = logMessage' K.DebugS $ K.logStr msg

-- | log a Text with severity = Info
logInfo :: (LogContext m) => Text -> m ()
logInfo msg = K.logItemM Nothing K.InfoS $ K.logStr msg

-- | log a Text with severity = Notice
logNotice :: (LogContext m) => Text -> m ()
logNotice msg = K.logItemM Nothing K.NoticeS $ K.logStr msg

-- | log a Text with severity = Warning
logWarning :: (LogContext m) => Text -> m ()
logWarning msg = K.logItemM Nothing K.WarningS $ K.logStr msg

-- | log a Text with severity = Error
logError :: (LogContext m) => Text -> m ()
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
setupLogging :: MonadIO m => LoggerConfig -> m LoggingHandler
setupLogging lc = do
    lh <- liftIO $ Internal.newConfig lc
    scribes <- liftIO $ meta lh lc
    liftIO $ Internal.registerBackends lh scribes
    return lh
      where
        -- returns a list of: (name, Scribe, finalizer)
        meta :: LoggingHandler -> LoggerConfig -> IO [(T.Text, K.Scribe)]
        meta _lh _lc = do
            -- setup scribes according to configuration
            let lhs = _lc ^. lcLoggerTree ^. ltHandlers ^.. each
                basepath = _lc ^. lcBasePath
                sevfilter = _lc ^. lcLoggerTree ^. ltNamedSeverity
                -- default rotation parameters: max. 24 hours, max. 10 files kept, max. size 5 MB
                rotation = fromMaybe (RotationParameters { _rpMaxAgeHours=24,
                                                           _rpKeepFilesNum=10,
                                                           _rpLogLimitBytes=5*1000*1000 })
                                     (_lc ^. lcRotation)
            forM lhs (\lh -> case (lh ^. lhBackend) of
                    FileJsonBE -> do
                        let bp = fromMaybe "." basepath
                            fp = fromMaybe "node.json" $ lh ^. lhFpath
                            fdesc = Internal.mkFileDescription bp fp
                            nm = lh ^. lhName
                        scribe <- mkJsonFileScribe
                                      rotation
                                      sevfilter
                                      fdesc
                                      (fromMaybe Debug $ lh ^. lhMinSeverity)
                                      K.V0
                        return (nm, scribe)
                    FileTextBE -> do
                        let bp = fromMaybe "." basepath
                            fp = (fromMaybe "node.log" $ lh ^. lhFpath)
                            fdesc = Internal.mkFileDescription bp fp
                            nm = lh ^. lhName
                        scribe <- mkTextFileScribe
                                      rotation
                                      sevfilter
                                      fdesc
                                      True
                                      (fromMaybe Debug $ lh ^. lhMinSeverity)
                                      K.V0
                        return (nm, scribe)
                    StdoutBE -> do
                        scribe <- mkStdoutScribe
                                      sevfilter
                                      (fromMaybe Debug $ lh ^. lhMinSeverity)
                                      K.V0
                        return (lh ^. lhName, scribe)
                    StderrBE -> do
                        scribe <- mkStderrScribe
                                      sevfilter
                                      (fromMaybe Debug $ lh ^. lhMinSeverity)
                                      K.V0
                        return (lh ^. lhName, scribe)
                    DevNullBE -> do
                        scribe <- mkDevNullScribe _lh
                                      sevfilter
                                      (fromMaybe Debug $ lh ^. lhMinSeverity)
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
usingLoggerName lh name action = usingLoggerNames lh [name] action
usingLoggerNames :: LoggingHandler -> [LoggerName] -> LogContextT IO a -> IO a
usingLoggerNames lh names action = do
    mayle <- Internal.getLogEnv lh
    case mayle of
            Nothing -> error "logging not yet initialized. Abort."
            Just le -> K.runKatipContextT le () (Internal.s2knames names) $ action

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
loggerBracket :: (MonadMask m, MonadIO m) => LoggingHandler -> LoggerName -> LogContextT m a -> m a
loggerBracket lh name action = do
    mayle <- liftIO $ Internal.getLogEnv lh
    case mayle of
            Nothing -> error "logging not yet initialized. Abort."
            Just le -> bracket (return le) finalizer body
    where
      finalizer le_ = void $ liftIO $ K.closeScribes le_
      body le_ = K.runKatipContextT le_ () (Internal.s2kname name) $ action


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

   >>> lc0 <- return $ defaultInteractiveConfiguration Info
   >>> newlt <- return $ lc0 ^. lcLoggerTree & ltNamedSeverity .~ Data.HashMap.Strict.fromList [("cardano-sl.silent", Error)]
   >>> lc <- return $ lc0 & lcLoggerTree .~ newlt
   >>> lh <- setupLogging lc
   >>> usingLoggerName lh "silent" $ do { logWarning "you won't see this!" }
   >>> usingLoggerName lh "verbose" $ do { logWarning "now you read this!" }
-}

-- | Equivalent to katip's logItem without the `Katip m` constraint
logItem'
    :: (KC.LogItem a, MonadIO m)
    => a
    -> KC.Namespace
    -> K.LogEnv
    -> Maybe TH.Loc
    -> KC.Severity
    -> KC.LogStr
    -> m ()
logItem' a ns env loc sev msg = do
    liftIO $ do
      item <- K.Item
        <$> pure (env ^. KC.logEnvApp)
        <*> pure (env ^. KC.logEnvEnv)
        <*> pure sev
        <*> (KC.mkThreadIdText <$> myThreadId)
        <*> pure (env ^. KC.logEnvHost)
        <*> pure (env ^. KC.logEnvPid)
        <*> pure a
        <*> pure msg
        <*> (env ^. KC.logEnvTimer)
        <*> pure ((env ^. KC.logEnvApp) <> ns)
        <*> pure loc
      forM_ (elems (env ^. KC.logEnvScribes)) $
          \ (KC.ScribeHandle _ shChan) -> atomically (KC.tryWriteTBQueue shChan (KC.NewItem item))
