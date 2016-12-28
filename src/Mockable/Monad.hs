{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mockable.Monad (
    MonadMockable(..)
    ) where

import Mockable.Class
import Mockable.Concurrent  (Delay(..))

-- | Bunch of Mockable-constraints.
type MonadMockable m
    = ( Mockable Delay m
      --, Mockable SharedAtomic m
      --, Mockable Fork m
      --, Mockable Bracket m
      --, Mockable Channel.Channel m
      --, Mockable Throw m
      --, Mockable Catch m
      )
