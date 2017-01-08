{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Mockable.Fail (

      Fail (..)
    , fail

    ) where

import Prelude hiding (fail)

import Mockable.Class (MFunctor' (hoist'), Mockable (liftMockable))

data Fail (m :: * -> *) (t :: *) where
    Fail :: String -> Fail m t

instance MFunctor' Fail m n where
    hoist' _ (Fail s) = Fail s

fail :: ( Mockable Fail m ) => String -> m a
fail reason = liftMockable $ Fail reason
