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
    , withSharedAtomic

    ) where

import           Mockable.Class (MFunctor' (hoist'), Mockable (liftMockable))

type family SharedAtomicT (m :: * -> *) :: * -> *

data SharedAtomic (m :: * -> *) (t :: *) where
    NewSharedAtomic :: t -> SharedAtomic m (SharedAtomicT m t)
    ModifySharedAtomic :: SharedAtomicT m s -> (s -> m (s, t)) -> SharedAtomic m t
    ReadSharedAtomic :: SharedAtomicT m t -> SharedAtomic m t

instance (SharedAtomicT n ~ SharedAtomicT m) => MFunctor' SharedAtomic m n where
    hoist' _ (NewSharedAtomic t)               = NewSharedAtomic t
    hoist' nat (ModifySharedAtomic var update) = ModifySharedAtomic var (\s -> nat $ update s)
    hoist' _ (ReadSharedAtomic sat)            = ReadSharedAtomic sat

{-# INLINE newSharedAtomic #-}
newSharedAtomic :: ( Mockable SharedAtomic m ) => t -> m (SharedAtomicT m t)
newSharedAtomic t = liftMockable $ NewSharedAtomic t

{-# INLINE readSharedAtomic #-}
readSharedAtomic :: ( Mockable SharedAtomic m ) => SharedAtomicT m t -> m t
readSharedAtomic sat = liftMockable $ ReadSharedAtomic sat

{-# INLINE modifySharedAtomic #-}
modifySharedAtomic
    :: ( Mockable SharedAtomic m )
    => SharedAtomicT m s
    -> (s -> m (s, t))
    -> m t
modifySharedAtomic sat f = liftMockable $ ModifySharedAtomic sat f

{-# INLINE withSharedAtomic #-}
withSharedAtomic
    :: ( Mockable SharedAtomic m )
    => SharedAtomicT m s
    -> (s -> m t)
    -> m t
withSharedAtomic sat f = liftMockable $ ModifySharedAtomic sat g
    where
    g s = fmap ((,) s) (f s)
