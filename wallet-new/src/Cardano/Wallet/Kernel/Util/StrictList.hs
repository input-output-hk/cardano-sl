{-# LANGUAGE DeriveFunctor #-}

-- | Strict lists
--
-- Intended for qualified import
module Cardano.Wallet.Kernel.Util.StrictList (
    StrictList(..)
  , take
  ) where

import           Universum hiding (take)

import           Data.SafeCopy (base, deriveSafeCopy)

data StrictList a = Nil | Cons !a !(StrictList a)
  deriving (Eq, Ord, Show, Functor)

deriveSafeCopy 1 'base ''StrictList

instance Semigroup (StrictList a) where
    Nil       <> ys = ys
    Cons x xs <> ys = Cons x (xs <> ys)

instance Monoid (StrictList a) where
    mempty  = Nil
    mappend = (<>)

take :: Int -> StrictList a -> StrictList a
take 0 _           = Nil
take _ Nil         = Nil
take n (Cons x xs) = Cons x (take (n - 1) xs)
