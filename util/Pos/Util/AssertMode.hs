{-# LANGUAGE CPP #-}

-- | Ability to do something only in a specific mode (determined at
-- compile-time).

module Pos.Util.AssertMode
       ( inAssertMode
       ) where

import           Universum

-- | This function performs checks at compile-time for different actions.
-- May slowdown implementation. To disable such checks (especially in benchmarks)
-- one should compile with: @stack build --flag cardano-sl-core:-asserts@
inAssertMode :: Applicative m => m a -> m ()
#ifdef ASSERTS_ON
inAssertMode x = x *> pure ()
#else
inAssertMode _ = pure ()
#endif
{-# INLINE inAssertMode #-}
