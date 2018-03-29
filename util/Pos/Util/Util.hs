{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Pos.Util.Util
       (
       -- * Cool stuff
         type (~>)
       -- * Exceptions/errors
       , maybeThrow
       , eitherToThrow
       , liftEither
       , leftToPanic
       , toAesonError
       , aesonError
       , toCborError
       , cborError
       , toTemplateHaskellError
       , templateHaskellError
       , toParsecError
       , parsecError
       , toCerealError
       , cerealError
       , DisallowException

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
       , logException
       , bracketWithLogging

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

       , tMeasureLog
       , tMeasureIO
       , timed
       ) where

import           Universum

import qualified Codec.CBOR.Decoding as CBOR
import           Control.Concurrent (threadDelay)
import qualified Control.Exception.Safe as E
import           Control.Lens (Getting, Iso', coerced, foldMapOf, ( # ))
import           Control.Monad.Except (MonadError, throwError)
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
import qualified Data.Serialize as Cereal
import           Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.Time.Units (Microsecond, toMicroseconds, fromMicroseconds)
import qualified Ether
import           Ether.Internal (HasLens (..))
import qualified Formatting as F
import           GHC.TypeLits (ErrorMessage (..))
import qualified Language.Haskell.TH as TH
import qualified Prelude
import           Serokell.Util (listJson)
import           Serokell.Util.Exceptions ()
import           System.Wlog (LoggerName, WithLogger, logDebug, logError, logInfo, usingLoggerName)
import qualified Text.Megaparsec as P

----------------------------------------------------------------------------
-- Cool stuff
----------------------------------------------------------------------------

type f ~> g = forall x. f x -> g x

----------------------------------------------------------------------------
-- Exceptions/errors
----------------------------------------------------------------------------

maybeThrow :: (MonadThrow m, Exception e) => e -> Maybe a -> m a
maybeThrow e = maybe (throwM e) pure

-- | Throw exception (in 'MonadThrow') or return result depending on
-- what is stored in 'Either'
eitherToThrow
    :: (MonadThrow m, Exception e)
    => Either e a -> m a
eitherToThrow = either throwM pure

-- | 'liftEither' from mtl-2.2.2.
-- TODO: use mtl version after we start using 2.2.2.
liftEither
    :: (MonadError e m)
    => Either e a -> m a
liftEither = either throwError pure

-- | Partial function which calls 'error' with meaningful message if
-- given 'Left' and returns some value if given 'Right'.
-- Intended usage is when you're sure that value must be right.
leftToPanic :: Buildable a => Text -> Either a b -> b
leftToPanic msgPrefix = either (error . mappend msgPrefix . pretty) identity

-- | This unexported helper is used to define conversions to 'MonadFail'
-- forced on us by external APIs. I also used underscores in its name, so don't
-- you think about exporting it -- define a specialized helper instead.
--
-- This must be the only place in our codebase where we allow 'MonadFail'.
external_api_fail :: MonadFail m => Either Text ~> m
external_api_fail = either (fail . toString) return

-- | Convert an 'Either'-encoded failure to an 'aeson' parser failure. The
-- return monad is intentionally specialized because we avoid 'MonadFail'.
toAesonError :: Either Text ~> A.Parser
toAesonError = external_api_fail

aesonError :: Text -> A.Parser a
aesonError = toAesonError . Left

-- | Convert an 'Either'-encoded failure to a 'cborg' decoder failure. The
-- return monad is intentionally specialized because we avoid 'MonadFail'.
toCborError :: Either Text ~> CBOR.Decoder s
toCborError = external_api_fail

cborError :: Text -> CBOR.Decoder s a
cborError = toCborError . Left

-- | Convert an 'Either'-encoded failure to a 'TH' Q-monad failure. The
-- return monad is intentionally specialized because we avoid 'MonadFail'.
toTemplateHaskellError :: Either Text ~> TH.Q
toTemplateHaskellError = external_api_fail

templateHaskellError :: Text -> TH.Q a
templateHaskellError = toTemplateHaskellError . Left

-- | Convert an 'Either'-encoded failure to a 'cereal' failure. The
-- return monad is intentionally specialized because we avoid 'MonadFail'.
toCerealError :: Either Text ~> Cereal.Get
toCerealError = external_api_fail

cerealError :: Text -> Cereal.Get a
cerealError = toCerealError . Left

-- | Convert an 'Either'-encoded failure to a 'megaparsec' failure. The
-- return monad is intentionally specialized because we avoid 'MonadFail'.
toParsecError :: P.Stream s => Either Text ~> P.ParsecT e s m
toParsecError = external_api_fail

parsecError :: P.Stream s => Text -> P.ParsecT e s m a
parsecError = toParsecError . Left

type family DisallowException t :: ErrorMessage where
    DisallowException t =
             'ShowType t ':<>:
             'Text " intentionally doesn't have an 'Exception' instance."
        ':$$: 'Text
             "This type shouldn't be thrown as a runtime exception."
        ':$$: 'Text
             "If you want to throw it, consider defining your own exception \
             \type with a constructor storing a value of this type."
        ':$$: 'Text
             "See the exception handling guidelines for more details."
        ':$$: 'Text ""

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
    toAesonError . readEither @String <=<
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

-- | Catch and log an exception, then rethrow it
logException :: LoggerName -> IO a -> IO a
logException name = E.handleAsync (\e -> handler e >> E.throw e)
  where
    handler :: E.SomeException -> IO ()
    handler exc = do
        let message = "logException: " <> pretty exc
        usingLoggerName name (logError message) `E.catchAny` \loggingExc -> do
            putStrLn message
            putStrLn $
                "logException failed to use logging: " <> pretty loggingExc

-- | 'bracket' which logs given message after acquiring the resource
-- and before calling the callback with 'Info' severity.
bracketWithLogging ::
       (MonadMask m, WithLogger m)
    => Text
    -> m a
    -> (a -> m b)
    -> (a -> m c)
    -> m c
bracketWithLogging msg acquire release = bracket acquire release . addLogging
  where
    addLogging callback resource = do
        logInfo $ "<bracketWithLogging:before> " <> msg
        callback resource <*
            logInfo ("<bracketWithLogging:after> " <> msg)

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

-- | 'tMeasure' with 'logDebug'.
tMeasureLog :: (MonadIO m, WithLogger m) => Text -> m a -> m a
tMeasureLog label = fmap fst . tMeasure logDebug label

-- | 'tMeasure' with 'putText'. For places you don't have
-- 'WithLogger' constraint.
tMeasureIO :: (MonadIO m) => Text -> m a -> m a
tMeasureIO label = fmap fst . tMeasure putText label

timed :: (MonadIO m, WithLogger m) => Text -> m a -> m (a, Microsecond)
timed = tMeasure logDebug

-- | Takes the first time sample, executes action (forcing its
-- result), takes the second time sample, logs it.
tMeasure :: (MonadIO m) => (Text -> m ()) -> Text -> m a -> m (a, Microsecond)
tMeasure logAction label action = do
    before <- liftIO getCurrentTime
    !x <- action
    after <- liftIO getCurrentTime
    let diff :: NominalDiffTime
        diff = after `diffUTCTime` before
        d0 :: Integer
        d0 = round $ 10000 * toRational diff
    let d1 = d0 `div` 10
    let d2 = d0 `mod` 10
    logAction $ "tMeasure " <> label <> ": " <> show d1 <> "." <> show d2 <> "ms"
    pure (x, fromMicroseconds (round $ 1000000 * toRational diff))
