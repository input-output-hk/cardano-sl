{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Mockable.Exception (

      Bracket(..)
    , bracket
    , bracketWithException
    , finally

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
    Bracket :: m r -> (r -> m b) -> (r -> m c) -> Bracket m c
    BracketWithException
        :: ( Exception e )
        => m r
        -> (r -> Maybe e -> m b)
        -> (r -> m c)
        -> Bracket m c

instance MFunctor' Bracket m n where
    hoist' nat (Bracket acq rel act) =
      Bracket (nat acq) (\r -> nat $ rel r) (\r -> nat $ act r)
    hoist' nat (BracketWithException acq rel act) =
      BracketWithException (nat acq) (\r e -> nat $ rel r e) (\r -> nat $ act r)

bracket :: ( Mockable Bracket m ) => m r -> (r -> m b) -> (r -> m c) -> m c
bracket acquire release act = liftMockable $ Bracket acquire release act

bracketWithException
    :: ( Mockable Bracket m, Exception e )
    => m r
    -> (r -> Maybe e -> m b)
    -> (r -> m c)
    -> m c
bracketWithException acquire release act = liftMockable $ BracketWithException acquire release act

finally :: ( Mockable Bracket m ) => m a -> m b -> m a
finally act end = bracket (return ()) (const end) (const act)

data Throw (m :: * -> *) (t :: *) where
    Throw :: Exception e => e -> Throw m t

instance MFunctor' Throw m n where
    hoist' _ (Throw e) = Throw e

throw :: ( Mockable Throw m ) => Exception e => e -> m t
throw e = liftMockable $ Throw e

data Catch (m :: * -> *) (t :: *) where
    Catch :: Exception e => m t -> (e -> m t) -> Catch m t

instance MFunctor' Catch m n where
    hoist' nat (Catch act handleE) = Catch (nat act) (\e -> nat $ handleE e)

catch :: ( Mockable Catch m, Exception e ) => m t -> (e -> m t) -> m t
catch action handler = liftMockable $ Catch action handler

catchAll :: ( Mockable Catch m ) => m t -> (SomeException -> m t) -> m t
catchAll = catch

handle :: ( Mockable Catch m, Exception e ) => (e -> m t) -> m t -> m t
handle = flip catch

handleAll :: ( Mockable Catch m ) => (SomeException -> m t) -> m t -> m t
handleAll = handle

try :: ( Mockable Catch m, Exception e ) => m t -> m (Either e t)
try = handle (return . Left) . fmap Right
