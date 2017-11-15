{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies      #-}

-- | Miscellaneous unclassified utility functions.

module Pos.Util
       (
         module Pos.Util.Util
       -- * Stuff for testing and benchmarking
       , module Pos.Util.Arbitrary
       , module Pos.Util.TimeLimit
       -- * Concurrency helpers
       , module Pos.Util.Concurrent
       -- * Futures
       , module Pos.Util.Future

       , module Pos.Util.Filesystem
       , module Pos.Util.Lens
       , module Pos.Util.Some

       -- * Various
       , mconcatPair
       , microsecondsToUTC

       -- * NonEmpty
       , neZipWith3
       , neZipWith4
       , spanSafe

       ) where

import           Universum hiding (finally)

import           Data.List (span, zipWith3, zipWith4)
import           Data.Ratio ((%))
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.Time.Units (Microsecond, toMicroseconds)

import           Pos.Util.Arbitrary
import           Pos.Util.Concurrent
import           Pos.Util.Filesystem
import           Pos.Util.Future
import           Pos.Util.Lens
import           Pos.Util.Some
import           Pos.Util.TimeLimit
import           Pos.Util.Util

-- | Specialized version of 'mconcat' (or 'Data.Foldable.fold')
-- for restricting type to list of pairs.
mconcatPair :: (Monoid a, Monoid b) => [(a, b)] -> (a, b)
mconcatPair = mconcat

microsecondsToUTC :: Microsecond -> UTCTime
microsecondsToUTC = posixSecondsToUTCTime . fromRational . (% 1000000) . toMicroseconds

----------------------------------------------------------------------------
-- NonEmpty
----------------------------------------------------------------------------

neZipWith3 :: (x -> y -> z -> q) -> NonEmpty x -> NonEmpty y -> NonEmpty z -> NonEmpty q
neZipWith3 f (x :| xs) (y :| ys) (z :| zs) = f x y z :| zipWith3 f xs ys zs

neZipWith4 ::
       (x -> y -> i -> z -> q)
    -> NonEmpty x
    -> NonEmpty y
    -> NonEmpty i
    -> NonEmpty z
    -> NonEmpty q
neZipWith4 f (x :| xs) (y :| ys) (i :| is) (z :| zs) = f x y i z :| zipWith4 f xs ys is zs

-- | Makes a span on the list, considering tail only. Predicate has
-- list head as first argument. Used to take non-null prefix that
-- depends on the first element.
spanSafe :: (a -> a -> Bool) -> NonEmpty a -> (NonEmpty a, [a])
spanSafe p (x:|xs) = let (a,b) = span (p x) xs in (x:|a,b)
