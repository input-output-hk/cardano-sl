{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Mockable.SharedExclusive (

      SharedExclusiveT
    , SharedExclusive(..)
    , newSharedExclusive
    , putSharedExclusive
    , tryPutSharedExclusive
    , takeSharedExclusive
    , modifySharedExclusive
    , readSharedExclusive

    ) where

import           Mockable.Class (MFunctor' (hoist'), Mockable (liftMockable))

type family SharedExclusiveT (m :: * -> *) :: * -> *

data SharedExclusive (m :: * -> *) (t :: *) where
    NewSharedExclusive :: SharedExclusive m (SharedExclusiveT m t)
    PutSharedExclusive :: SharedExclusiveT m t -> t -> SharedExclusive m ()
    TakeSharedExclusive :: SharedExclusiveT m t -> SharedExclusive m t
    ModifySharedExclusive :: SharedExclusiveT m t -> (t -> m (t, r)) -> SharedExclusive m r
    TryPutSharedExclusive :: SharedExclusiveT m t -> t -> SharedExclusive m Bool

instance (SharedExclusiveT n ~ SharedExclusiveT m) => MFunctor' SharedExclusive m n where
    hoist' _ NewSharedExclusive              = NewSharedExclusive
    hoist' _ (PutSharedExclusive var t)      = PutSharedExclusive var t
    hoist' _ (TakeSharedExclusive var)       = TakeSharedExclusive var
    hoist' nat (ModifySharedExclusive var f) = ModifySharedExclusive var (nat . f)
    hoist' _ (TryPutSharedExclusive var t)   = TryPutSharedExclusive var t

{-# INLINE newSharedExclusive #-}
newSharedExclusive :: ( Mockable SharedExclusive m ) => m (SharedExclusiveT m t)
newSharedExclusive = liftMockable $ NewSharedExclusive

{-# INLINE putSharedExclusive #-}
putSharedExclusive :: ( Mockable SharedExclusive m ) => SharedExclusiveT m t -> t -> m ()
putSharedExclusive var t = liftMockable $ PutSharedExclusive var t

{-# INLINE takeSharedExclusive #-}
takeSharedExclusive :: ( Mockable SharedExclusive m ) => SharedExclusiveT m t -> m t
takeSharedExclusive var = liftMockable $ TakeSharedExclusive var

{-# INLINE modifySharedExclusive #-}
modifySharedExclusive :: ( Mockable SharedExclusive m ) => SharedExclusiveT m t -> (t -> m (t, r)) -> m r
modifySharedExclusive var f = liftMockable $ ModifySharedExclusive var f

{-# INLINE readSharedExclusive #-}
readSharedExclusive :: ( Mockable SharedExclusive m ) => SharedExclusiveT m t -> m t
readSharedExclusive var = liftMockable $ ModifySharedExclusive var (\t -> return (t, t))

{-# INLINE tryPutSharedExclusive #-}
tryPutSharedExclusive :: ( Mockable SharedExclusive m ) => SharedExclusiveT m t -> t -> m Bool
tryPutSharedExclusive var t = liftMockable $ TryPutSharedExclusive var t
