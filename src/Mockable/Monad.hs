{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mockable.Monad
    ( MonadMockable
    ) where

import Mockable.Channel      (Channel)
import Mockable.Class
import Mockable.Concurrent   (Delay, Fork)
import Mockable.Exception    (Bracket, Catch, Throw)
import Mockable.SharedAtomic (SharedAtomic)

-- | Bunch of Mockable-constraints.
type MonadMockable m
    = ( Mockable Delay m
      , Mockable SharedAtomic m
      , Mockable Fork m
      , Mockable Bracket m
      , Mockable Channel m
      , Mockable Throw m
      , Mockable Catch m
      )
