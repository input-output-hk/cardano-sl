{-# LANGUAGE DeriveFunctor #-}

-- | Strict lists
--
-- Intended for qualified import
module Cardano.Wallet.Kernel.Util.StrictList (
    StrictList(..)
  , singleton
  , take
  , drop
  , dropWhile
  , toMaybe
  ) where

import           Universum hiding (drop, dropWhile, take)

import           Pos.Core.Chrono

import           Data.SafeCopy (SafeCopy (..), base, contain, deriveSafeCopy,
                     safeGet, safePut)

data StrictList a = Nil | Cons !a !(StrictList a)
  deriving (Eq, Ord, Show, Functor)

deriveSafeCopy 1 'base ''StrictList

instance SafeCopy a => SafeCopy (NewestFirst StrictList a) where
    getCopy = contain $ NewestFirst <$> safeGet
    putCopy (NewestFirst xs) = contain $ safePut xs

instance SafeCopy a => SafeCopy (OldestFirst StrictList a) where
    getCopy = contain $ OldestFirst <$> safeGet
    putCopy (OldestFirst xs) = contain $ safePut xs

instance Semigroup (StrictList a) where
    Nil       <> ys = ys
    Cons x xs <> ys = Cons x (xs <> ys)

instance Monoid (StrictList a) where
    mempty  = Nil
    mappend = (<>)

singleton :: a -> StrictList a
singleton x = Cons x Nil

take :: Int -> StrictList a -> StrictList a
take 0 _           = Nil
take _ Nil         = Nil
take n (Cons x xs) = Cons x (take (n - 1) xs)

drop :: Int -> StrictList a -> StrictList a
drop 0 xs          = xs
drop _ Nil         = Nil
drop n (Cons _ xs) = drop (n - 1) xs

dropWhile :: (a -> Bool) -> StrictList a -> StrictList a
dropWhile p (Cons x xs)
            | p x       = dropWhile p xs
            | otherwise = Cons x xs
dropWhile _ Nil         = Nil

toMaybe :: StrictList a -> Maybe a
toMaybe Nil        = Nothing
toMaybe (Cons x _) = Just x
