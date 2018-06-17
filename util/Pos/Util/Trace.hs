{-# LANGUAGE RankNTypes #-}

module Pos.Util.Trace
    ( Trace (..)
    , TraceIO
    , natTrace
    , trace
    , traceWith
    , noTrace
    , setupLogging
    -- * trace setup
    --, stdoutTrace
    --, stdoutTraceConcurrent
    , logTrace
    -- * log messages
    , logDebug
    , logInfo
    , logWarning
    , logNotice
    , logError
    , Log.Severity (..)
    ) where

import           Universum hiding (trace, newEmptyMVar)
import           Data.Functor.Contravariant (Contravariant (..), Op (..))
import qualified Pos.Util.Log as Log

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
setupLogging :: Log.LoggerConfig -> Log.LoggerName -> IO TraceIO
setupLogging lc ln = do
    lh <- Log.setupLogging lc
    return $ logTrace lh ln

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

-- | A 'Trace' that uses logging from @Pos.Util.Log@
logTrace :: Log.LoggingHandler -> Log.LoggerName -> TraceIO
logTrace lh loggerName = Trace $ Op $ \(severity, txt) ->
    Log.usingLoggerName lh loggerName $ Log.logMessage severity txt

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
