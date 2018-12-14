{-# LANGUAGE UndecidableInstances #-}

-- | Misc QuickCheck utilities
module Util.QuickCheck (
    LiftQuickCheck(..)
  ) where

import           Universum

import           Test.QuickCheck

{-------------------------------------------------------------------------------
  Lift QuickCheck actions
-------------------------------------------------------------------------------}

-- | Lift QuickCheck actions
class Monad m => LiftQuickCheck m where
   -- | Run a QuickCheck computation
  liftQuickCheck :: Gen x -> m x

instance LiftQuickCheck IO where
  liftQuickCheck = generate

instance (LiftQuickCheck m, MonadTrans t, Monad (t m)) => LiftQuickCheck (t m) where
  liftQuickCheck = lift . liftQuickCheck
