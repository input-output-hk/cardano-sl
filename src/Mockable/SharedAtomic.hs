{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Mockable.SharedAtomic (

      SharedAtomicT
    , SharedAtomic(..)
    , newSharedAtomic
    , readSharedAtomic
    , modifySharedAtomic

    ) where

import           Mockable.Class (MFunctor' (hoist'), Mockable (liftMockable))

type family SharedAtomicT (m :: * -> *) :: * -> *

data SharedAtomic (m :: * -> *) (t :: *) where
    NewSharedAtomic :: t -> SharedAtomic m (SharedAtomicT m t)
    ModifySharedAtomic :: SharedAtomicT m s -> (s -> m (s, t)) -> SharedAtomic m t

instance (SharedAtomicT n ~ SharedAtomicT m) => MFunctor' SharedAtomic m n where
    hoist' _ (NewSharedAtomic t)            = NewSharedAtomic t
    hoist' nat (ModifySharedAtomic var mod) = ModifySharedAtomic var (\s -> nat $ mod s)

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
