{-# LANGUAGE DeriveFunctor #-}

-- | Strict, non-empty lists
--
-- Intended for qualified import
module Cardano.Wallet.Kernel.Util.StrictNonEmpty (
    StrictNonEmpty(..)
  , singleton
  , head
  , (<|)
  , take
  ) where

import           Universum hiding ((:|), head, take)

import           Pos.Core.Chrono

import           Data.SafeCopy (SafeCopy (..), base, contain, deriveSafeCopy,
                     safeGet, safePut)

import           Cardano.Wallet.Kernel.Util.StrictList (StrictList)
import qualified Cardano.Wallet.Kernel.Util.StrictList as SL

data StrictNonEmpty a = (:|) !a !(StrictList a)
  deriving (Eq, Ord, Show, Functor)

instance SafeCopy a => SafeCopy (NewestFirst StrictNonEmpty a) where
    getCopy = contain $ NewestFirst <$> safeGet
    putCopy (NewestFirst xs) = contain $ safePut xs

instance SafeCopy a => SafeCopy (OldestFirst StrictNonEmpty a) where
    getCopy = contain $ OldestFirst <$> safeGet
    putCopy (OldestFirst xs) = contain $ safePut xs

deriveSafeCopy 1 'base ''StrictNonEmpty

instance Semigroup (StrictNonEmpty a) where
    (x :| xs) <> (y :| ys) = x :| (xs <> (SL.Cons y ys))

singleton :: a -> StrictNonEmpty a
singleton a = a :| mempty

head :: Lens' (StrictNonEmpty a) a
head f (x :| xs) = (\x' -> x' :| xs) <$> f x

(<|) :: a -> StrictNonEmpty a -> StrictNonEmpty a
x <| (x' :| xs) = x :| (SL.Cons x' xs)

-- | Take @n@ elements from the list
--
-- @n@ should be greater than or equal to 1.
take :: Int -> StrictNonEmpty a -> StrictNonEmpty a
take 0 _         = error "StrictNonEmpty.take: cannot take 0"
take k (x :| xs) = x :| SL.take (k - 1) xs
