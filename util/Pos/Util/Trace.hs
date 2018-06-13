{-# LANGUAGE RankNTypes #-}

module Pos.Util.Trace
    ( Trace (..)
    , TraceIO
    , natTrace
    , trace
    , traceWith
    , logTrace
    , noTrace
    -- * log messages
    , stdoutTrace
    , stdoutTraceConcurrent
    , Log.Severity (..)
    ) where

import           Universum hiding (trace, newEmptyMVar)
import           Control.Concurrent.MVar (newEmptyMVar, withMVar)
import           Data.Functor.Contravariant (Contravariant (..), Op (..))
import qualified Data.Text.IO as TIO
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

{- FIXME maybe use this function to init logging
-- | setup logging and return a Trace
setupLogging :: Log.LoggerConfig -> TraceIO
setupLogging _ = logTrace "from setup"
-}

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

-- | Trace lines to stdout, without concurrency control.
stdoutTrace :: Trace IO Text
stdoutTrace = Trace $ Op $ TIO.putStrLn

-- | Trace lines to stdout, with concurrency control.
stdoutTraceConcurrent :: IO (Trace IO Text)
stdoutTraceConcurrent = do
    mv <- newEmptyMVar :: IO (MVar ())
    let traceIt = \txt -> withMVar mv $ \_ -> TIO.putStrLn txt
    pure $ Trace $ Op $ traceIt

-- | A 'Trace' that uses logging
logTrace :: Log.LoggerName -> TraceIO
logTrace loggerName = Trace $ Op $ \(severity, txt) ->
    Log.usingLoggerName loggerName $ Log.logMessage severity txt

{-
logDebug :: TraceIO -> Trace IO Text
logDebug lt = contramap ((,) Log.Debug) lt

logInfo :: TraceIO -> Trace IO Text
logInfo lt = contramap ((,) Log.Info) lt

logWarning :: TraceIO -> Trace IO Text
logWarning lt = contramap ((,) Log.Warning) lt

logNotice :: TraceIO -> Trace IO Text
logNotice lt = contramap ((,) Log.Notice) lt

logError :: TraceIO -> Trace IO Text
logError lt = contramap ((,) Log.Error) lt
-}
