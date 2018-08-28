{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}

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
  , reverse
  , last
  , init
  , splitLast
  , toList
  ) where

import           Universum hiding (drop, dropWhile, init, last, reverse, take,
                     toList)

import qualified Data.Foldable
import           Test.QuickCheck (Arbitrary (..), arbitrary)
import           Data.SafeCopy (SafeCopy (..), base, contain, deriveSafeCopy,
                     safeGet, safePut)
import           Pos.Core.Chrono

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

instance One (StrictList a) where
    type OneItem (StrictList a) = a
    one = singleton

instance Foldable StrictList where
    foldr e f = Data.Foldable.foldr e f . toList

toList :: StrictList a -> [a]
toList Nil         = []
toList (Cons x xs) = x : toList xs

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

reverse :: StrictList a -> StrictList a
reverse = go Nil
  where
    go :: StrictList a -> StrictList a -> StrictList a
    go acc Nil         = acc
    go acc (Cons x xs) = go (Cons x acc) xs

last :: StrictList a -> Maybe a
last = fmap snd . splitLast

init :: StrictList a -> Maybe (StrictList a)
init = fmap fst . splitLast

-- | Split off the last element
splitLast :: forall a. StrictList a -> Maybe (StrictList a, a)
splitLast = \case
    Nil       -> Nothing
    Cons x xs -> Just $ go Nil x xs
  where
    go :: StrictList a -- Everything-but-the-last element seen so far
       -> a            -- The last element seen so far
       -> StrictList a -- Elements not yet seen
       -> (StrictList a, a)
    go acc lastElem Nil         = (reverse acc, lastElem)
    go acc lastElem (Cons x xs) = go (Cons lastElem acc) x xs

fromList :: [a] -> StrictList a
fromList [] = Nil
fromList (a:r) = Cons a $ fromList r

instance Arbitrary a => Arbitrary (StrictList a) where
    arbitrary = do
        ls <- arbitrary
        pure $ fromList ls
