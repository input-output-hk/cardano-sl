{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Mockable.Exception (

      Bracket(..)
    , bracket

    , Throw(..)
    , throw

    ) where

import Mockable.Class
import Control.Exception (Exception)

data Bracket (m :: * -> *) (t :: *) where
    Bracket :: m r -> (r -> m b) -> (r -> m c) -> Bracket m c

bracket :: ( Mockable Bracket m ) => m r -> (r -> m b) -> (r -> m c) -> m c
bracket acquire release act = liftMockable $ Bracket acquire release act

data Throw (m :: * -> *) (t :: *) where
    Throw :: Exception e => e -> Throw m t

throw :: ( Mockable Throw m ) => Exception e => e -> m t
throw e = liftMockable $ Throw e
