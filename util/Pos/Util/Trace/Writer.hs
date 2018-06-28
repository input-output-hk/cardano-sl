-- | A 'Trace' which uses a writer monad transformer.

module Pos.Util.Trace.Writer
    ( writerTrace
    ) where

import           Control.Monad.Writer.Class (MonadWriter, tell)
import           Data.Functor.Contravariant (Op (..))
import           Pos.Util.Trace (Trace (..))
import           Prelude

writerTrace :: MonadWriter a m => Trace m a
writerTrace = Trace $ Op $ tell
