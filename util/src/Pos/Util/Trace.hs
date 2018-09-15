{-# LANGUAGE RankNTypes #-}

module Pos.Util.Trace
    ( Trace (..)
    , TraceIO
    , natTrace
    , trace
    , traceWith
    , noTrace
    , stdoutTrace
    -- TODO put wlog tracing into its own module.
    , wlogTrace
    , Wlog.Severity (..)
    -- * trace setup
    , setupLogging
    , wsetupLogging
    , logTrace
    -- * log messages
    , logDebug
    , logInfo
    , logWarning
    , logNotice
    , logError
    ) where

import           Data.Functor.Contravariant (Contravariant (..), Op (..))
import qualified Data.Text.IO as TIO
import qualified Pos.Util.Log as Log
import qualified Pos.Util.Wlog as Wlog
import           Universum hiding (trace)

-- | Abstracts logging.
newtype Trace m s = Trace
    { runTrace :: Op (m ()) s
    }

type TraceIO = Trace IO (Log.Severity, Text)

instance Contravariant (Trace m) where
    contramap f = Trace . contramap f . runTrace

natTrace :: (forall x . m x -> n x) -> Trace m s -> Trace n s
natTrace nat (Trace (Op tr)) = Trace $ Op $ nat . tr

-- | setup logging and return a Trace
setupLogging :: MonadIO m
             => Log.LoggerConfig
             -> Log.LoggerName
             -> IO (Trace m (Log.Severity, Text))
setupLogging lc ln = do
    lh <- Log.setupLogging lc
    return $ logTrace lh ln

wsetupLogging :: Wlog.LoggerConfig
              -> Wlog.LoggerName
              -> IO (Trace IO (Wlog.Severity, Text))
wsetupLogging lc ln = do
    Wlog.setupLogging lc
    return $ wlogTrace ln

trace :: Trace m s -> s -> m ()
trace = getOp . runTrace

-- | Alias to 'trace' so that you don't clash with 'Debug.trace' in case it's
-- imported (Universum exports it).
traceWith :: Trace m s -> s -> m ()
traceWith = trace

-- | A 'Trace' that ignores everything. NB this actually turns off logging: it
-- doesn't force the logged messages.
noTrace :: Applicative m => Trace m a
noTrace = Trace $ Op $ const (pure ())

-- | 'Trace' to stdout.
stdoutTrace :: Trace IO Text
stdoutTrace = Trace $ Op $ TIO.putStrLn

-- | A 'Trace' that uses log-warper.
wlogTrace :: Wlog.LoggerName -> Trace IO (Wlog.Severity, Text)
wlogTrace loggerName = Trace $ Op $ \(severity, txt) ->
    Wlog.usingLoggerName loggerName $ Wlog.logMessage severity txt

-- | A 'Trace' that uses logging from @Pos.Util.Log@
logTrace :: MonadIO m
         => Log.LoggingHandler
         -> Log.LoggerName
         -> Trace m (Log.Severity, Text)
logTrace lh loggerName = Trace $ Op $ \(severity, txt) ->
    liftIO $ Log.usingLoggerName lh loggerName $ Log.logMessage severity txt

logDebug :: TraceIO -> Trace IO Text
logDebug = contramap ((,) Log.Debug)

logInfo :: TraceIO -> Trace IO Text
logInfo = contramap ((,) Log.Info)

logWarning :: TraceIO -> Trace IO Text
logWarning = contramap ((,) Log.Warning)

logNotice :: TraceIO -> Trace IO Text
logNotice = contramap ((,) Log.Notice)

logError :: TraceIO -> Trace IO Text
logError = contramap ((,) Log.Error)
