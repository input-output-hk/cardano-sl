{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mockable.Monad (
    MonadMockable(..)
    ) where

import Mockable.Class
import Mockable.Channel         (Channel)
import Mockable.Concurrent      (Delay, RepeatForever, Fork)
import Mockable.Exception       (Bracket, Throw, Catch)
import Mockable.SharedAtomic    (SharedAtomic)

-- | Bunch of Mockable-constraints.
type MonadMockable m
    = ( Mockable Delay m
      , Mockable RepeatForever m
      , Mockable SharedAtomic m
      , Mockable Fork m
      , Mockable Bracket m
      , Mockable Channel m
      , Mockable Throw m
      , Mockable Catch m
      )
