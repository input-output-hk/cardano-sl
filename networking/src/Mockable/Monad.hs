{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mockable.Monad
    ( MonadMockable
    ) where

import           Control.Exception.Safe (MonadMask)
import           Mockable.Channel (Channel)
import           Mockable.Class (Mockable)
import           Mockable.Concurrent (Async, Concurrently, Delay, MyThreadId,
                     ThreadId)
import           Mockable.CurrentTime (CurrentTime)
import           Mockable.Metrics (Metrics)
import           Mockable.SharedAtomic (SharedAtomic)
import           Mockable.SharedExclusive (SharedExclusive)

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
