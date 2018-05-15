
module Pos.Util.Trace
    ( Trace (..)
    , trace
    , traceWith
    , noTrace
    , stdoutTrace
    , logTrace
    , Log.Severity (..)
    ) where

import           Universum hiding (trace)
import           Data.Functor.Contravariant (Contravariant (..), Op (..))
import qualified Data.Text.IO as TIO
import qualified Pos.Util.Log as Log

-- | Abstracts logging.
newtype Trace m s = Trace
    { runTrace :: Op (m ()) s
    }

instance Contravariant (Trace m) where
    contramap f = Trace . contramap f . runTrace

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

-- | A 'Trace' that uses logging
logTrace :: Log.LoggerName -> Trace IO (Log.Severity, Text)
logTrace loggerName = Trace $ Op $ \(severity, txt) ->
    Log.usingLoggerName Log.Debug loggerName $ Log.logMessage severity txt
