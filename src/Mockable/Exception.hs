{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Mockable.Exception (

      Bracket(..)
    , bracket
    , finally

    , Throw(..)
    , throw

    , Catch(..)
    , catch
    , catchAll
    , handle
    , handleAll

    ) where

import Mockable.Class
import Control.Exception (Exception, SomeException)

data Bracket (m :: * -> *) (t :: *) where
    Bracket :: m r -> (r -> m b) -> (r -> m c) -> Bracket m c

bracket :: ( Mockable Bracket m ) => m r -> (r -> m b) -> (r -> m c) -> m c
bracket acquire release act = liftMockable $ Bracket acquire release act

finally :: ( Mockable Bracket m ) => m a -> m b -> m a
finally act end = bracket (return ()) (const end) (const act)

data Throw (m :: * -> *) (t :: *) where
    Throw :: Exception e => e -> Throw m t

throw :: ( Mockable Throw m ) => Exception e => e -> m t
throw e = liftMockable $ Throw e

data Catch (m :: * -> *) (t :: *) where
    Catch :: Exception e => m t -> (e -> m t) -> Catch m t

catch :: ( Mockable Catch m, Exception e ) => m t -> (e -> m t) -> m t
catch action handler = liftMockable $ Catch action handler

catchAll :: ( Mockable Catch m ) => m t -> (SomeException -> m t) -> m t
catchAll = catch

handle :: ( Mockable Catch m, Exception e ) => (e -> m t) -> m t -> m t
handle = flip catch

handleAll :: ( Mockable Catch m ) => (SomeException -> m t) -> m t -> m t
handleAll = handle
