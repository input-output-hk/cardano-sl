{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mockable.Monad
    ( MonadMockable
    ) where

import           Mockable.Channel (Channel)
import           Mockable.Class (Mockable)
import           Mockable.Concurrent (Async, Concurrently, Delay, Fork, ThreadId)
import           Mockable.CurrentTime (CurrentTime)
import           Mockable.Exception (Bracket, Catch, Throw)
import           Mockable.Metrics (Metrics)
import           Mockable.SharedAtomic (SharedAtomic)
import           Mockable.SharedExclusive (SharedExclusive)

-- | Bunch of Mockable-constraints.
type MonadMockable m
    = ( Mockable Delay m
      , Mockable SharedAtomic m
      , Mockable Fork m
      , Mockable CurrentTime m
      , Mockable Concurrently m
      , Mockable Async m
      , Mockable Bracket m
      , Mockable Channel m
      , Mockable Throw m
      , Mockable Catch m
      , Ord (ThreadId m)
      , Show (ThreadId m)
      , Mockable SharedExclusive m
      , Mockable Metrics m
      )
