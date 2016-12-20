{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Mockable.SharedAtomic (

      SharedAtomicT
    , SharedAtomic(..)
    , newSharedAtomic
    , readSharedAtomic
    , modifySharedAtomic

    ) where

import Mockable.Class

type family SharedAtomicT (m :: * -> *) :: * -> *

data SharedAtomic (m :: * -> *) (t :: *) where
    NewSharedAtomic :: t -> SharedAtomic m (SharedAtomicT m t)
    ModifySharedAtomic :: SharedAtomicT m s -> (s -> m (s, t)) -> SharedAtomic m t

newSharedAtomic :: ( Mockable SharedAtomic m ) => t -> m (SharedAtomicT m t)
newSharedAtomic t = liftMockable $ NewSharedAtomic t

readSharedAtomic :: ( Mockable SharedAtomic m ) => SharedAtomicT m t -> m t
readSharedAtomic sat = modifySharedAtomic sat (\x -> pure (x, x))

modifySharedAtomic
    :: ( Mockable SharedAtomic m )
    => SharedAtomicT m s
    -> (s -> m (s, t))
    -> m t
modifySharedAtomic sat f = liftMockable $ ModifySharedAtomic sat f
