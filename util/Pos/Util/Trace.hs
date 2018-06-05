{-# LANGUAGE RankNTypes #-}

module Pos.Util.Trace
    ( Trace (..)
    , natTrace
    , trace
    , traceWith
    , noTrace
    , stdoutTrace
    , stdoutTraceConcurrent
    ) where

import           Universum hiding (trace, newEmptyMVar)
import           Control.Concurrent.MVar (newEmptyMVar, withMVar)
import           Data.Functor.Contravariant (Contravariant (..), Op (..))
import qualified Data.Text.IO as TIO

-- | Abstracts logging.
newtype Trace m s = Trace
    { runTrace :: Op (m ()) s
    }

instance Contravariant (Trace m) where
    contramap f = Trace . contramap f . runTrace

natTrace :: (forall x . m x -> n x) -> Trace m s -> Trace n s
natTrace nat (Trace (Op tr)) = Trace $ Op $ nat . tr

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
