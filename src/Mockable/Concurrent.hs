{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Mockable.Concurrent (

    ThreadId
  , Fork(..)
  , fork
  , myThreadId
  , killThread

  , RunInUnboundThread(..)
  , runInUnboundThread

  ) where

import Mockable.Class

type family ThreadId (m :: * -> *) :: *

data Fork m t where
    Fork :: m () -> Fork m (ThreadId m)
    MyThreadId :: Fork m (ThreadId m)
    KillThread :: ThreadId m -> Fork m ()

fork :: ( Mockable Fork m ) => m () -> m (ThreadId m)
fork term = liftMockable $ Fork term

myThreadId :: ( Mockable Fork m ) => m (ThreadId m)
myThreadId = liftMockable MyThreadId

killThread :: ( Mockable Fork m ) => ThreadId m -> m ()
killThread tid = liftMockable $ KillThread tid

data RunInUnboundThread m t where
    RunInUnboundThread :: m t -> RunInUnboundThread m t

runInUnboundThread :: ( Mockable RunInUnboundThread m ) => m t -> m t
runInUnboundThread m = liftMockable $ RunInUnboundThread m
