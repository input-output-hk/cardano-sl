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
  , last
  , init
  , splitLast
  , prependList
  , toList
  ) where

import           Universum hiding ((:|), head, init, last, take, toList)

import           Data.SafeCopy (SafeCopy (..), base, contain, deriveSafeCopy,
                     safeGet, safePut)
import qualified Data.Foldable
import           Pos.Core.Chrono

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

instance One (StrictNonEmpty a) where
    type OneItem (StrictNonEmpty a) = a
    one = singleton

instance Foldable StrictNonEmpty where
    foldr e f = Data.Foldable.foldr e f . toList

toList :: StrictNonEmpty a -> [a]
toList (x :| xs) = x : SL.toList xs

singleton :: a -> StrictNonEmpty a
singleton a = a :| mempty

head :: Lens' (StrictNonEmpty a) a
head f (x :| xs) = (\x' -> x' :| xs) <$> f x

init :: StrictNonEmpty a -> StrictList a
init = fst . splitLast

last :: StrictNonEmpty a -> a
last = snd . splitLast

-- | @splitLast ([a .. y] ++ [z]) = ([a .. y], z)@
splitLast :: StrictNonEmpty a -> (StrictList a, a)
splitLast (x :| xs) =
    case SL.splitLast xs of
      Nothing               -> (SL.Nil, x)
      Just (xsInit, xsLast) -> (SL.Cons x xsInit, xsLast)

(<|) :: a -> StrictNonEmpty a -> StrictNonEmpty a
x <| (x' :| xs) = x :| (SL.Cons x' xs)

-- | Take @n@ elements from the list
--
-- @n@ should be greater than or equal to 1.
take :: Int -> StrictNonEmpty a -> StrictNonEmpty a
take 0 _         = error "StrictNonEmpty.take: cannot take 0"
take k (x :| xs) = x :| SL.take (k - 1) xs

prependList :: StrictList a -> StrictNonEmpty a -> StrictNonEmpty a
prependList SL.Nil         ys        = ys
prependList (SL.Cons x xs) (y :| ys) = x :| (xs <> SL.Cons y ys)
