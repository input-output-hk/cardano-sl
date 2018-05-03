{-# LANGUAGE LambdaCase #-}

module Pos.Util.Log
       ( Severity(..)
       , LogContext
       , LogContextT
       , loggerBracket
       , logDebug
       , logInfo
       , logNotice
       , logWarning
       , logError
       ) where

import           Universum

import qualified Katip                      as K


-- | abstract libraries' severity
data Severity = Debug | Info | Warning | Notice | Error


-- | alias - pretend not to depend on katip
type LogContext = K.KatipContext
type LogContextT = K.KatipContextT

-- | log a Text with severity = Debug
logDebug :: (LogContext m {-, HasCallStack -}) => Text -> m ()
logDebug msg = K.logItemM Nothing K.DebugS $ K.logStr msg

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
    hScribe <- K.mkHandleScribe K.ColorIfTerminal stdout (sev2klog minSev) K.V0
    K.registerScribe "stdout" hScribe K.defaultScribeSettings =<< K.initLogEnv (s2kname name) "production"

-- | bracket logging
loggerBracket :: Severity -> Text -> LogContextT IO a -> IO a
loggerBracket minSev name f = do
    bracket (setupLogging minSev name) K.closeScribes $
      \le -> K.runKatipContextT le () "bracket" $ f

{-
test1 :: IO ()
test1 = do
    loggerBracket Info "testtest" $ do
        logInfo "This is a message"

test2 :: IO ()
test2 = do
    loggerBracket Info "testtest" $ do
        logDebug "This is a DEBUG message"
-}
