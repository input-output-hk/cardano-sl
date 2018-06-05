-- | A 'Trace' which uses a writer monad transformer.

module Pos.Util.Trace.Writer
    ( writerTrace
    ) where

import           Prelude
import           Control.Monad.Writer.Class (MonadWriter, tell)
import           Data.Functor.Contravariant (Op (..))
import           Pos.Util.Trace (Trace (..))

writerTrace :: MonadWriter a m => Trace m a
writerTrace = Trace $ Op $ tell
