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
       , mappendPair
       , mconcatPair
       , eitherToVerRes
       , readerToState
       , diffDoubleMap
       , mapEither
       , microsecondsToUTC

       -- * NonEmpty
       , neZipWith3
       , neZipWith4
       , spanSafe

       ) where

import           Universum hiding (finally)

import           Data.Either (rights)
import           Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import           Data.List (span, zipWith3, zipWith4)
import           Data.Ratio ((%))
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.Time.Units (Microsecond, toMicroseconds)
import           Serokell.Util (VerificationRes (..))
-- SafeCopy instance for HashMap
import           Serokell.AcidState ()

import           Pos.Util.Arbitrary
import           Pos.Util.Concurrent
import           Pos.Util.Filesystem
import           Pos.Util.Future
import           Pos.Util.Lens
import           Pos.Util.Some
import           Pos.Util.TimeLimit
import           Pos.Util.Undefined ()
import           Pos.Util.Util

-- | Specialized version of 'mappend' for restricted to pair type.
mappendPair :: (Monoid a, Monoid b) => (a, b) -> (a, b) -> (a, b)
mappendPair = mappend

-- | Specialized version of 'mconcat' (or 'Data.Foldable.fold')
-- for restricting type to list of pairs.
mconcatPair :: (Monoid a, Monoid b) => [(a, b)] -> (a, b)
mconcatPair = mconcat

-- | Convert (Reader s) to any (MonadState s)
readerToState
    :: MonadState s m
    => Reader s a -> m a
readerToState = gets . runReader

-- | Remove elements which are in 'b' from 'a'
diffDoubleMap
    :: forall k1 k2 v.
       (Eq k1, Eq k2, Hashable k1, Hashable k2)
    => HashMap k1 (HashMap k2 v)
    -> HashMap k1 (HashMap k2 v)
    -> HashMap k1 (HashMap k2 v)
diffDoubleMap a b = HM.foldlWithKey' go mempty a
  where
    go :: HashMap k1 (HashMap k2 v)
       -> k1
       -> HashMap k2 v
       -> HashMap k1 (HashMap k2 v)
    go res extKey internalMap =
        case HM.lookup extKey b of
            Nothing -> HM.insert extKey internalMap res
            Just internalMapB ->
                let diff = internalMap `HM.difference` internalMapB
                in if null diff
                       then res
                       else HM.insert extKey diff res

mapEither :: (a -> Either b c) -> [a] -> [c]
mapEither f = rights . map f

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

----------------------------------------------------------------------------
-- Deserialized wrapper
----------------------------------------------------------------------------

eitherToVerRes :: Either Text a -> VerificationRes
eitherToVerRes (Left errors) = if T.null errors then VerFailure []
                               else VerFailure $ T.split (==';') errors
eitherToVerRes (Right _ )    = VerSuccess
