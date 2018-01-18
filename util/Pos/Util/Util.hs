{-# LANGUAGE PolyKinds  #-}
{-# LANGUAGE RankNTypes #-}

module Pos.Util.Util
       (
       -- * Exceptions/errors
         maybeThrow
       , eitherToFail
       , eitherToThrow
       , leftToPanic
       , logException

       -- * Ether
       , ether
       , Ether.TaggedTrans
       , HasLens(..)
       , HasLens'
       , lensOf'
       , lensOfProxy

       -- * Lifting monads
       , PowerLift(..)

       -- * MinMax
       , MinMax(..)
       , _MinMax
       , mkMinMax
       , minMaxOf

       -- * Aeson
       , parseJSONWithRead

       -- * NonEmpty
       , neZipWith3
       , neZipWith4
       , spanSafe
       , takeLastNE

       -- * Logging helpers
       , buildListBounds
       , multilineBounds

       -- * Misc
       , mconcatPair
       , microsecondsToUTC
       , Sign (..)
       , getKeys
       , sortWithMDesc
       , dumpSplices
       , histogram
       , median
       , (<//>)
       , divRoundUp
       , sleep

       ) where

import           Universum

import           Control.Concurrent (threadDelay)
import qualified Control.Exception.Safe as E
import           Control.Lens (Getting, Iso', coerced, foldMapOf, ( # ))
import           Control.Monad.Trans.Class (MonadTrans)
import           Data.Aeson (FromJSON (..))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import           Data.HashSet (fromMap)
import           Data.List (span, zipWith3, zipWith4)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import           Data.Ratio ((%))
import qualified Data.Semigroup as Smg
import           Data.Time.Clock (NominalDiffTime, UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.Time.Units (Microsecond, toMicroseconds)
import qualified Ether
import           Ether.Internal (HasLens (..))
import qualified Formatting as F
import qualified Language.Haskell.TH as TH
import qualified Prelude
import           Serokell.Util (listJson)
import           Serokell.Util.Exceptions ()
import           System.Wlog (LoggerName, logError, usingLoggerName)

----------------------------------------------------------------------------
-- Exceptions/errors
----------------------------------------------------------------------------

maybeThrow :: (MonadThrow m, Exception e) => e -> Maybe a -> m a
maybeThrow e = maybe (throwM e) pure

-- | Fail or return result depending on what is stored in 'Either'.
eitherToFail :: (MonadFail m, ToString s) => Either s a -> m a
eitherToFail = either (fail . toString) pure

-- | Throw exception or return result depending on what is stored in 'Either'
eitherToThrow
    :: (MonadThrow m, Exception e)
    => Either e a -> m a
eitherToThrow = either throwM pure

-- | Partial function which calls 'error' with meaningful message if
-- given 'Left' and returns some value if given 'Right'.
-- Intended usage is when you're sure that value must be right.
leftToPanic :: Buildable a => Text -> Either a b -> b
leftToPanic msgPrefix = either (error . mappend msgPrefix . pretty) identity

-- | Catch and log an exception, then rethrow it
logException :: LoggerName -> IO a -> IO a
logException name = E.handleAsync (\e -> handler e >> E.throw e)
  where
    handler :: E.SomeException -> IO ()
    handler = usingLoggerName name . logError . pretty

----------------------------------------------------------------------------
-- Ether
----------------------------------------------------------------------------

-- | Make a Reader or State computation work in an Ether transformer. Useful
-- to make lenses work with Ether.
ether :: trans m a -> Ether.TaggedTrans tag trans m a
ether = Ether.TaggedTrans

-- | Convenient shortcut for 'HasLens' constraint when lens is to the
-- same type as the tag.
type HasLens' s a = HasLens a s a

-- | Version of 'lensOf' which is used when lens is to the same type
-- as the tag.
lensOf' :: forall a s. HasLens' s a => Lens' s a
lensOf' = lensOf @a

-- | Version of 'lensOf' which uses proxy.
lensOfProxy :: forall proxy tag a b. HasLens tag a b => proxy tag -> Lens' a b
lensOfProxy _ = lensOf @tag

----------------------------------------------------------------------------
-- PowerLift
----------------------------------------------------------------------------

class PowerLift m n where
    powerLift :: m a -> n a

instance {-# OVERLAPPING #-} PowerLift m m where
    powerLift = identity

instance (MonadTrans t, PowerLift m n, Monad n) => PowerLift m (t n) where
  powerLift = lift . powerLift @m @n

----------------------------------------------------------------------------
-- MinMax
----------------------------------------------------------------------------

newtype MinMax a = MinMax (Smg.Option (Smg.Min a, Smg.Max a))
    deriving (Monoid)

_MinMax :: Iso' (MinMax a) (Maybe (a, a))
_MinMax = coerced

mkMinMax :: a -> MinMax a
mkMinMax a = _MinMax # Just (a, a)

minMaxOf :: Getting (MinMax a) s a -> s -> Maybe (a, a)
minMaxOf l = view _MinMax . foldMapOf l mkMinMax

----------------------------------------------------------------------------
-- Aeson
----------------------------------------------------------------------------

-- | Parse a value represented as a 'show'-ed string in JSON.
parseJSONWithRead :: Read a => A.Value -> A.Parser a
parseJSONWithRead =
    either (fail . toString) pure . readEither @String <=<
    parseJSON

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

-- | Takes last N elements of the list
takeLastNE :: Int -> NonEmpty a -> [a]
takeLastNE n = reverse . NE.take n . NE.reverse

----------------------------------------------------------------------------
-- Logging helpers
----------------------------------------------------------------------------

-- | Formats two values as first and last elements of a list
buildListBounds :: Buildable a => F.Format r (NonEmpty a -> r)
buildListBounds = F.later formatList
  where
    formatList (x:|[]) = F.bprint ("[" F.% F.build F.% "]") x
    formatList xs = F.bprint ("[" F.% F.build F.% ".." F.% F.build F.% "]")
        (NE.head xs)
        (NE.last xs)

-- | Formats only start and the end of the list according to the maximum size
multilineBounds :: Buildable a => Int -> F.Format r (NonEmpty a -> r)
multilineBounds maxSize = F.later formatList
 where
   formatList xs = if length xs <= maxSize'
       then F.bprint listJson xs
       else F.bprint
          ("First " F.% F.int F.% ": " F.% listJson F.% "\nLast " F.% F.int F.% ": " F.% listJson)
          half
          (NE.take half xs)
          remaining
          (takeLastNE remaining xs)
   maxSize' = max 2 maxSize -- splitting list into two with maximum size below 2 doesn't make sense
   half = maxSize' `div` 2
   remaining = maxSize' - half

----------------------------------------------------------------------------
-- Misc
----------------------------------------------------------------------------

-- | Specialized version of 'mconcat' (or 'Data.Foldable.fold')
-- for restricting type to list of pairs.
mconcatPair :: (Monoid a, Monoid b) => [(a, b)] -> (a, b)
mconcatPair = mconcat

microsecondsToUTC :: Microsecond -> UTCTime
microsecondsToUTC = posixSecondsToUTCTime . fromRational . (% 1000000) . toMicroseconds

data Sign = Plus | Minus

-- | Create HashSet from HashMap's keys
getKeys :: HashMap k v -> HashSet k
getKeys = fromMap . void

-- | Use some monadic action to evaluate priority of value and sort a
-- list of values based on this priority. The order is descending
-- because I need it.
sortWithMDesc :: (Monad m, Ord b) => (a -> m b) -> [a] -> m [a]
sortWithMDesc f = fmap (map fst . sortWith (Down . snd)) . mapM f'
  where
    f' x = (x, ) <$> f x

-- | Concatenates two url parts using regular slash '/'.
-- E.g. @"./dir/" <//> "/file" = "./dir/file"@.
(<//>) :: String -> String -> String
(<//>) lhs rhs = lhs' ++ "/" ++ rhs'
  where
    isSlash = (== '/')
    lhs' = reverse $ dropWhile isSlash $ reverse lhs
    rhs' = dropWhile isSlash rhs

-- | To be used with paging of any kind.
-- The pages should contain N elements (we use 10 by default):
-- - 1  - 10
-- - 11 - 20
-- - 21 - 30
divRoundUp :: Integral a => a -> a -> a
divRoundUp a b = (a + b - 1) `div` b

-- | Print splices generated by a TH splice (the printing will happen during
-- compilation, as a GHC warning). Useful for debugging.
--
-- For instance, you can dump splices generated with 'makeLenses' by
-- replacing a top-level invocation of 'makeLenses' in your code with:
--
-- @dumpSplices $ makeLenses ''Foo@
--
dumpSplices :: TH.DecsQ -> TH.DecsQ
dumpSplices x = do
    ds <- x
    let code = Prelude.lines (TH.pprint ds)
    TH.reportWarning ("\n" ++ Prelude.unlines (map ("    " ++) code))
    return ds

-- | Count elements in a list.
histogram :: forall a. Ord a => [a] -> Map a Int
histogram = foldl' step M.empty
  where
    step :: Map a Int -> a -> Map a Int
    step m x = M.insertWith (+) x 1 m

median :: Ord a => NonEmpty a -> a
median l = NE.sort l NE.!! middle
  where
    len = NE.length l
    middle = (len - 1) `div` 2

{-| Sleep for the given duration

    A numeric literal argument is interpreted as seconds.  In other words,
    @(sleep 2.0)@ will sleep for two seconds.
    Taken from http://hackage.haskell.org/package/turtle, BSD3 licence.
-}
sleep :: MonadIO m => NominalDiffTime -> m ()
sleep n = liftIO (threadDelay (truncate (n * 10^(6::Int))))
