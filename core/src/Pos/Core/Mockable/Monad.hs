{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pos.Core.Mockable.Monad
    ( MonadMockable
    ) where

import           Prelude

import           Control.Exception.Safe (MonadMask)
import           Pos.Core.Mockable.Channel (Channel)
import           Pos.Core.Mockable.Class (Mockable)
import           Pos.Core.Mockable.Concurrent (Async, Concurrently, Delay,
                     MyThreadId, ThreadId)
import           Pos.Core.Mockable.CurrentTime (CurrentTime)
import           Pos.Core.Mockable.Metrics (Metrics)
import           Pos.Core.Mockable.SharedAtomic (SharedAtomic)
import           Pos.Core.Mockable.SharedExclusive (SharedExclusive)

-- | Bunch of Mockable-constraints. Intentionally doesn't contain some
-- constraints, e. g. 'Mockable Fork', because usually they should be
-- avoided.
type MonadMockable m
    = ( Mockable Delay m
      , Mockable SharedAtomic m
      , Mockable MyThreadId m
      , Mockable CurrentTime m
      , Mockable Concurrently m
      , Mockable Async m
      , Mockable Channel m
      , MonadMask m
      , Ord (ThreadId m)
      , Show (ThreadId m)
      , Mockable SharedExclusive m
      , Mockable Metrics m
      )
