{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Mockable.Exception (

      Bracket(..)
    , bracket
    , bracket_
    , bracketWithException
    , finally
    , onException
    , mask
    , mask_
    , unsafeUnmask

    , Throw(..)
    , throw

    , Catch(..)
    , catch
    , catchAll
    , handle
    , handleAll
    , try

    ) where

import           Control.Exception (Exception, SomeException)
import           Mockable.Class    (MFunctor' (hoist'), Mockable (liftMockable))

data Bracket (m :: * -> *) (t :: *) where
    Mask_ :: m a -> Bracket m a
    UnsafeUnmask :: m a -> Bracket m a
    Bracket :: m r -> (r -> m b) -> (r -> m c) -> Bracket m c
    BracketWithException
        :: ( Exception e )
        => m r
        -> (r -> Maybe e -> m b)
        -> (r -> m c)
        -> Bracket m c

instance MFunctor' Bracket m n where
    hoist' nat (Mask_ f) =
      Mask_ (nat f)
    hoist' nat (UnsafeUnmask f) =
      UnsafeUnmask (nat f)
    hoist' nat (Bracket acq rel act) =
      Bracket (nat acq) (\r -> nat $ rel r) (\r -> nat $ act r)
    hoist' nat (BracketWithException acq rel act) =
      BracketWithException (nat acq) (\r e -> nat $ rel r e) (\r -> nat $ act r)

{-# INLINE mask #-}
mask :: Mockable Bracket m => ((forall a. m a -> m a) -> m b) -> m b
mask f = mask_ (f unsafeUnmask)

{-# INLINE mask_ #-}
mask_ :: Mockable Bracket m => m a -> m a
mask_ act = liftMockable $ Mask_ act

{-# INLINE unsafeUnmask #-}
unsafeUnmask :: Mockable Bracket m => m a -> m a
unsafeUnmask act = liftMockable $ UnsafeUnmask act

{-# INLINE bracket #-}
bracket :: ( Mockable Bracket m ) => m r -> (r -> m b) -> (r -> m c) -> m c
bracket acquire release act = liftMockable $ Bracket acquire release act

{-# INLINE bracket_ #-}
bracket_ :: ( Mockable Bracket m ) => m r -> m b -> m c -> m c
bracket_ acquire release act = bracket acquire (const release) (const act)

{-# INLINE bracketWithException #-}
bracketWithException
    :: ( Mockable Bracket m, Exception e )
    => m r
    -> (r -> Maybe e -> m b)
    -> (r -> m c)
    -> m c
bracketWithException acquire release act = liftMockable $ BracketWithException acquire release act

{-# INLINE finally #-}
finally :: ( Mockable Bracket m ) => m a -> m b -> m a
finally act end = bracket (return ()) (const end) (const act)

{-# INLINE onException #-}
onException :: ( Mockable Catch m, Mockable Throw m ) => m a -> m b -> m a
onException act ex = act `catch` (\(e :: SomeException) -> ex >> throw e)

data Throw (m :: * -> *) (t :: *) where
    Throw :: Exception e => e -> Throw m t

instance MFunctor' Throw m n where
    hoist' _ (Throw e) = Throw e

{-# INLINE throw #-}
throw :: ( Mockable Throw m ) => Exception e => e -> m t
throw e = liftMockable $ Throw e

data Catch (m :: * -> *) (t :: *) where
    Catch :: Exception e => m t -> (e -> m t) -> Catch m t

instance MFunctor' Catch m n where
    hoist' nat (Catch act handleE) = Catch (nat act) (\e -> nat $ handleE e)

{-# INLINE catch #-}
catch :: ( Mockable Catch m, Exception e ) => m t -> (e -> m t) -> m t
catch action handler = liftMockable $ Catch action handler

{-# INLINE catchAll #-}
catchAll :: ( Mockable Catch m ) => m t -> (SomeException -> m t) -> m t
catchAll = catch

{-# INLINE handle #-}
handle :: ( Mockable Catch m, Exception e ) => (e -> m t) -> m t -> m t
handle = flip catch

{-# INLINE handleAll #-}
handleAll :: ( Mockable Catch m ) => (SomeException -> m t) -> m t -> m t
handleAll = handle

{-# INLINE try #-}
try :: ( Mockable Catch m, Exception e ) => m t -> m (Either e t)
try = handle (return . Left) . fmap Right
