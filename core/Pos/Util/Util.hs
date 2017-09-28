{-# LANGUAGE CPP             #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TypeFamilies    #-}

module Pos.Util.Util
       (
       -- * Existential types
         Some(..)
       , Some1(..)
       , applySome
       , liftLensSome
       , liftGetterSome

       -- * Something
       , Sign (..)
       , maybeThrow
       , eitherToFail
       , eitherToThrow
       , getKeys
       , sortWithMDesc
       , leftToPanic
       , dumpSplices
       , histogram
       , (<//>)

       -- * Lenses
       , _neHead
       , _neTail
       , _neLast
       , postfixLFields

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

       -- * Asserts
       , inAssertMode

       -- * Filesystem & process utilities
       , ls
       , lstree
       , withTempDir
       , directory
       , sleep
       , withTempFile
       , withSystemTempFile

       -- * Coloring
       , colorizeDull

       -- * Aeson
       , parseJSONWithRead

       -- * Instances
       -- ** Lift Byte
       -- ** Lift HashMap
       -- ** FromJSON Byte, ToJSON Byte
       -- ** Hashable Byte
       -- ** MonadFail (Either s), assuming IsString s
       -- ** HasLoggerName (MonadPseudoRandom drg)

       -- ** Hashable
       -- *** Millisecond, Microsecond

       -- ** NFData
       -- *** Millisecond, Microsecond

       -- ** MonadRandom
       -- *** monad transformers
       -- *** Gen (from QuickCheck)

       -- ** Buildable
       -- *** "Data.Time.Units" types
       ) where

import           Universum
import           Unsafe                         (unsafeInit, unsafeLast)

import           Control.Concurrent             (myThreadId, threadDelay)
import           Control.Lens                   (ALens', Getter, Getting, Iso', LensRules,
                                                 cloneLens, coerced, foldMapOf, lensField,
                                                 lensRules, mappingNamer, to, ( # ))
import           Control.Monad.Base             (MonadBase)
import qualified Control.Monad.Catch            as MC
import           Control.Monad.Morph            (MFunctor (..))
import           Control.Monad.Trans.Class      (MonadTrans)
import           Control.Monad.Trans.Identity   (IdentityT (..))
import           Control.Monad.Trans.Lift.Local (LiftLocal (..))
import           Control.Monad.Trans.Resource   (MonadResource (..), ResourceT,
                                                 transResourceT)
import qualified Crypto.Random                  as Rand
import           Data.Aeson                     (FromJSON (..), ToJSON (..))
import qualified Data.Aeson                     as A
import qualified Data.Aeson.Types               as A
import           Data.Char                      (isAlphaNum)
import           Data.Hashable                  (Hashable (hashWithSalt))
import qualified Data.HashMap.Strict            as HM
import           Data.HashSet                   (fromMap)
import           Data.List                      (last)
import qualified Data.Map                       as M
import qualified Data.Semigroup                 as Smg
import           Data.Tagged                    (Tagged (Tagged))
import           Data.Text.Buildable            (build)
import           Data.Time                      (getCurrentTime)
import           Data.Time.Clock                (NominalDiffTime)
import           Data.Time.Units                (Attosecond, Day, Femtosecond, Fortnight,
                                                 Hour, Microsecond, Millisecond, Minute,
                                                 Nanosecond, Picosecond, Second, Week,
                                                 toMicroseconds)
import           Data.Typeable                  (typeRep)
import qualified Ether
import           Ether.Internal                 (HasLens (..))
import qualified Formatting                     as F
import qualified Language.Haskell.TH            as TH
import qualified Language.Haskell.TH.Syntax     as TH
import           Mockable                       (ChannelT, Counter, Distribution, Gauge,
                                                 MFunctor' (..), Mockable (..), Promise,
                                                 SharedAtomicT, SharedExclusiveT,
                                                 ThreadId)
import qualified Prelude
import           Serokell.Data.Memory.Units     (Byte, fromBytes, toBytes)
import qualified System.Console.ANSI            as ANSI
import           System.Directory               (canonicalizePath, createDirectory,
                                                 doesDirectoryExist,
                                                 getTemporaryDirectory, listDirectory,
                                                 removeDirectoryRecursive, removeFile)
import           System.FilePath                (normalise, pathSeparator, takeDirectory,
                                                 (</>))
import           System.IO                      (hClose, openTempFile)
import           System.Wlog                    (CanLog, HasLoggerName (..),
                                                 LoggerNameBox (..))
import qualified Test.QuickCheck                as QC
import           Test.QuickCheck.Monadic        (PropertyM (..))

----------------------------------------------------------------------------
-- Some
----------------------------------------------------------------------------

-- | Turn any constraint into an existential type! Example:
--
-- @
-- foo :: Some Show -> String
-- foo (Some s) = show s
-- @
data Some c where
    Some :: c a => a -> Some c

instance Show (Some Show) where
    show (Some s) = show s

-- | Like 'Some', but for @* -> *@ types – for instance, @Some1 Functor ()@
data Some1 c a where
    Some1 :: c f => f a -> Some1 c a

instance Functor (Some1 Functor) where
    fmap f (Some1 x) = Some1 (fmap f x)

-- | Apply a function requiring the constraint
applySome :: (forall a. c a => a -> r) -> (Some c -> r)
applySome f (Some x) = f x

-- | Turn a lens into something operating on 'Some'. Useful for many types
-- like 'HasDifficulty', 'IsHeader', etc.
liftLensSome :: (forall a. c a => ALens' a b)
             -> Lens' (Some c) b
liftLensSome l =
    \f (Some a) -> Some <$> cloneLens l f a

-- | Like 'liftLensSome', but for getters.
liftGetterSome :: (forall a. c a => Getting b a b) -> Getter (Some c) b
liftGetterSome l = \f (Some a) -> Some <$> to (view l) f a

----------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------

instance MonadReader r m => MonadReader r (PropertyM m) where
    ask = lift ask
    local f (MkPropertyM propertyM) =
        MkPropertyM $ \hole -> local f <$> propertyM hole

instance (TH.Lift k, TH.Lift v) => TH.Lift (HashMap k v) where
    lift x = let l = HM.toList x in [|HM.fromList l|]

instance Hashable Byte where
    hashWithSalt i = hashWithSalt i . toInteger

instance TH.Lift Byte where
    lift x = let b = toBytes x in [|fromBytes b :: Byte|]

instance FromJSON Byte where
    parseJSON = fmap fromBytes . parseJSON

instance ToJSON Byte where
    toJSON = toJSON . toBytes

instance IsString s => MonadFail (Either s) where
    fail = Left . fromString

instance Rand.DRG drg => HasLoggerName (Rand.MonadPseudoRandom drg) where
    getLoggerName = pure "MonadPseudoRandom"
    modifyLoggerName = flip const

instance {-# OVERLAPPABLE #-}
         (MonadTrans t, Functor (t m), Monad (t m), Rand.MonadRandom m)
         => Rand.MonadRandom (t m) where
    getRandomBytes = lift . Rand.getRandomBytes

instance Rand.MonadRandom QC.Gen where
    getRandomBytes n = do
        [a,b,c,d,e] <- replicateM 5 (QC.choose (minBound, maxBound))
        pure $ fst $ Rand.randomBytesGenerate n (Rand.drgNewTest (a,b,c,d,e))

instance Hashable Millisecond where
    hashWithSalt i a = hashWithSalt i (toInteger a)

instance Hashable Microsecond where
    hashWithSalt i a = hashWithSalt i (toInteger a)

instance NFData Millisecond where
    rnf ms = rnf (toInteger ms)

instance NFData Microsecond where
    rnf ms = rnf (toInteger ms)


----------------------------------------------------------------------------
-- Orphan Buildable instances for time-units
----------------------------------------------------------------------------

instance Buildable Attosecond  where build = build @String . show
instance Buildable Femtosecond where build = build @String . show
instance Buildable Picosecond  where build = build @String . show
instance Buildable Nanosecond  where build = build @String . show
instance Buildable Millisecond where build = build @String . show
instance Buildable Second      where build = build @String . show
instance Buildable Minute      where build = build @String . show
instance Buildable Hour        where build = build @String . show
instance Buildable Day         where build = build @String . show
instance Buildable Week        where build = build @String . show
instance Buildable Fortnight   where build = build @String . show

-- | Special case. We don't want to print greek letter mu in console because
-- it breaks things sometimes.
instance Buildable Microsecond where
    build = build . (++ "mcs") . show . toMicroseconds

----------------------------------------------------------------------------
-- MonadResource/ResourceT
----------------------------------------------------------------------------

instance LiftLocal ResourceT where
    liftLocal _ l f = hoist (l f)

instance {-# OVERLAPPABLE #-}
    (MonadResource m, MonadTrans t, Applicative (t m),
     MonadBase IO (t m), MonadIO (t m), MonadThrow (t m)) =>
        MonadResource (t m)
  where
    liftResourceT = lift . liftResourceT

-- TODO Move it to log-warper
instance CanLog m => CanLog (ResourceT m)
instance (Monad m, HasLoggerName m) => HasLoggerName (ResourceT m) where
    getLoggerName = lift getLoggerName
    modifyLoggerName = transResourceT . modifyLoggerName

----------------------------------------------------------------------------
-- Instances required by 'ether'
----------------------------------------------------------------------------

instance
    (Monad m, MonadTrans t, Monad (t m), CanLog m) =>
        CanLog (Ether.TaggedTrans tag t m)

instance (Monad m, CanLog m) => CanLog (IdentityT m)

instance
    (LiftLocal t, Monad m, HasLoggerName m) =>
        HasLoggerName (Ether.TaggedTrans tag t m)
  where
    getLoggerName = lift getLoggerName
    modifyLoggerName = liftLocal getLoggerName modifyLoggerName

instance
    (Monad m, HasLoggerName m) => HasLoggerName (IdentityT m)
  where
    getLoggerName = lift getLoggerName
    modifyLoggerName = liftLocal getLoggerName modifyLoggerName

deriving instance LiftLocal LoggerNameBox

instance {-# OVERLAPPABLE #-}
    (Monad m, MFunctor t) => MFunctor' t m n
  where
    hoist' = hoist

instance
    (Mockable d m, MFunctor' d (IdentityT m) m) =>
        Mockable d (IdentityT m)
  where
    liftMockable dmt = IdentityT $ liftMockable $ hoist' runIdentityT dmt

unTaggedTrans :: Ether.TaggedTrans tag t m a -> t m a
unTaggedTrans (Ether.TaggedTrans tma) = tma

instance
      (Mockable d (t m), Monad (t m),
       MFunctor' d (Ether.TaggedTrans tag t m) (t m)) =>
          Mockable d (Ether.TaggedTrans tag t m)
  where
    liftMockable dmt =
      Ether.TaggedTrans $ liftMockable $ hoist' unTaggedTrans dmt

type instance ThreadId (IdentityT m) = ThreadId m
type instance Promise (IdentityT m) = Promise m
type instance SharedAtomicT (IdentityT m) = SharedAtomicT m
type instance Counter (IdentityT m) = Counter m
type instance Distribution (IdentityT m) = Distribution m
type instance SharedExclusiveT (IdentityT m) = SharedExclusiveT m
type instance Gauge (IdentityT m) = Gauge m
type instance ChannelT (IdentityT m) = ChannelT m

type instance ThreadId (Ether.TaggedTrans tag t m) = ThreadId m
type instance Promise (Ether.TaggedTrans tag t m) = Promise m
type instance SharedAtomicT (Ether.TaggedTrans tag t m) = SharedAtomicT m
type instance Counter (Ether.TaggedTrans tag t m) = Counter m
type instance Distribution (Ether.TaggedTrans tag t m) = Distribution m
type instance SharedExclusiveT (Ether.TaggedTrans tag t m) = SharedExclusiveT m
type instance Gauge (Ether.TaggedTrans tag t m) = Gauge m
type instance ChannelT (Ether.TaggedTrans tag t m) = ChannelT m

----------------------------------------------------------------------------
-- Not instances
----------------------------------------------------------------------------

data Sign = Plus | Minus

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

-- | Partial function which calls 'error' with meaningful message if
-- given 'Left' and returns some value if given 'Right'.
-- Intended usage is when you're sure that value must be right.
leftToPanic :: Buildable a => Text -> Either a b -> b
leftToPanic msgPrefix = either (error . mappend msgPrefix . pretty) identity

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

class PowerLift m n where
    powerLift :: m a -> n a

instance {-# OVERLAPPING #-} PowerLift m m where
    powerLift = identity

instance (MonadTrans t, PowerLift m n, Monad n) => PowerLift m (t n) where
  powerLift = lift . powerLift @m @n

instance (Typeable s, Buildable a) => Buildable (Tagged s a) where
    build tt@(Tagged v) = F.bprint ("Tagged " F.% F.shown F.% " " F.% F.build) ts v
      where
        ts = typeRep proxy
        proxy = (const Proxy :: Tagged s a -> Proxy s) tt

-- | This function performs checks at compile-time for different actions.
-- May slowdown implementation. To disable such checks (especially in benchmarks)
-- one should compile with: @stack build --flag cardano-sl-core:-asserts@
inAssertMode :: Applicative m => m a -> m ()
#ifdef ASSERTS_ON
inAssertMode x = x *> pure ()
#else
inAssertMode _ = pure ()
#endif
{-# INLINE inAssertMode #-}

-- | Concatenates two url parts using regular slash '/'.
-- E.g. @"./dir/" <//> "/file" = "./dir/file"@.
(<//>) :: String -> String -> String
(<//>) lhs rhs = lhs' ++ "/" ++ rhs'
  where
    isSlash = (== '/')
    lhs' = reverse $ dropWhile isSlash $ reverse lhs
    rhs' = dropWhile isSlash rhs

----------------------------------------------------------------------------
-- Lenses
----------------------------------------------------------------------------

-- | Lens for the head of 'NonEmpty'.
--
-- We can't use '_head' because it doesn't work for 'NonEmpty':
-- <https://github.com/ekmett/lens/issues/636#issuecomment-213981096>.
-- Even if we could though, it wouldn't be a lens, only a traversal.
_neHead :: Lens' (NonEmpty a) a
_neHead f (x :| xs) = (:| xs) <$> f x

-- | Lens for the tail of 'NonEmpty'.
_neTail :: Lens' (NonEmpty a) [a]
_neTail f (x :| xs) = (x :|) <$> f xs

-- | Lens for the last element of 'NonEmpty'.
_neLast :: Lens' (NonEmpty a) a
_neLast f (x :| []) = (:| []) <$> f x
_neLast f (x :| xs) = (\y -> x :| unsafeInit xs ++ [y]) <$> f (unsafeLast xs)

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

postfixLFields :: LensRules
postfixLFields = lensRules & lensField .~ mappingNamer (\s -> [s++"_L"])

-- | Count elements in a list.
histogram :: forall a. Ord a => [a] -> Map a Int
histogram = foldl' step M.empty
  where
    step :: Map a Int -> a -> Map a Int
    step m x = M.insertWith (+) x 1 m

-- MinMax

newtype MinMax a = MinMax (Smg.Option (Smg.Min a, Smg.Max a))
    deriving (Monoid)

_MinMax :: Iso' (MinMax a) (Maybe (a, a))
_MinMax = coerced

mkMinMax :: a -> MinMax a
mkMinMax a = _MinMax # Just (a, a)

minMaxOf :: Getting (MinMax a) s a -> s -> Maybe (a, a)
minMaxOf l = view _MinMax . foldMapOf l mkMinMax

----------------------------------------------------------------------------
-- Filesystem & process utilities
----------------------------------------------------------------------------

-- | Lists all immediate children of the given directory, excluding "." and ".."
-- Returns all the files inclusive of the initial `FilePath`.
ls :: MonadIO m => FilePath -> m [FilePath]
ls initialFp = map ((</>) initialFp) <$> liftIO (listDirectory (normalise initialFp))

-- | Lists all recursive descendants of the given directory.
lstree :: MonadIO m => FilePath -> m [FilePath]
lstree fp = go mempty fp
  where
    consUniq :: FilePath -> [FilePath] -> [FilePath]
    consUniq x xs = if x /= fp then (x : xs) else xs

    go :: MonadIO m => [FilePath] -> FilePath -> m [FilePath]
    go !acc currentFp = do
        isDirectory <- liftIO (doesDirectoryExist currentFp)
        case isDirectory of
            True  -> ls currentFp >>= foldM go (consUniq currentFp acc)
            False -> return (consUniq currentFp acc)

-- | Creates a temporary directory, nuking it after the inner action completes,
-- even if an exception is raised.
withTempDir :: FilePath
            -- ^ Parent directory
            -> Text
            -- ^ Directory name template
            -> (FilePath -> IO a)
            -> IO a
withTempDir parentDir template = bracket acquire dispose
  where
    acquire :: IO FilePath
    acquire = do
        tid <- filter isAlphaNum . show <$> myThreadId
        now <- filter isAlphaNum . show <$> getCurrentTime
        pth <- canonicalizePath $ normalise $ parentDir </> (toString template <> tid <> now)
        createDirectory pth
        return pth

    dispose :: FilePath -> IO ()
    dispose = removeDirectoryRecursive

-- | Simple shim to emulate the behaviour of `Filesystem.Path.directory`,
-- which is a bit more lenient than `System.FilePath.takeDirectory`.
directory :: FilePath -> FilePath
directory "" = ""
directory f = case last f of
    x | x == pathSeparator -> f
    _ -> takeDirectory (normalise f)

{-| Sleep for the given duration

    A numeric literal argument is interpreted as seconds.  In other words,
    @(sleep 2.0)@ will sleep for two seconds.
    Taken from http://hackage.haskell.org/package/turtle, BSD3 licence.
-}
sleep :: MonadIO m => NominalDiffTime -> m ()
sleep n = liftIO (threadDelay (truncate (n * 10^(6::Int))))

-- | Return the absolute and canonical path to the system temporary
-- directory.
-- Taken from http://hackage.haskell.org/package/temporary, BSD3 licence.
getCanonicalTemporaryDirectory :: IO FilePath
getCanonicalTemporaryDirectory = getTemporaryDirectory >>= canonicalizePath

-- | Create, open, and use a temporary file in the system standard temporary directory.
--
-- The temp file is deleted after use.
--
-- Behaves exactly the same as 'withTempFile', except that the parent temporary directory
-- will be that returned by 'getCanonicalTemporaryDirectory'.
-- Taken from http://hackage.haskell.org/package/temporary, BSD3 licence.
withSystemTempFile :: (MonadIO m, MC.MonadMask m) =>
                      String   -- ^ File name template
                   -> (FilePath -> Handle -> m a) -- ^ Callback that can use the file
                   -> m a
withSystemTempFile template action = liftIO getCanonicalTemporaryDirectory >>= \tmpDir -> withTempFile tmpDir template action

-- | Create, open, and use a temporary file in the given directory.
--
-- The temp file is deleted after use.
-- Taken from http://hackage.haskell.org/package/temporary, BSD3 licence.
withTempFile :: (MonadIO m, MonadMask m)
             => FilePath
             -- ^ Parent directory to create the file in
             -> String
             -- ^ File name template
             -> (FilePath -> Handle -> m a)
             -- ^ Callback that can use the file
             -> m a
withTempFile tmpDir template action =
  MC.bracket
    (liftIO (openTempFile tmpDir template))
    (\(name, handle) -> liftIO (hClose handle >> ignoringIOErrors (removeFile name)))
    (uncurry action)
  where
     ignoringIOErrors :: MC.MonadCatch m => m () -> m ()
     ignoringIOErrors ioe = ioe `MC.catch` (\e -> const (return ()) (e :: Prelude.IOError))

----------------------------------------------------------------------------
-- Coloring
----------------------------------------------------------------------------

-- | Colorize text using 'ANSI.Dull' palete
-- (in contrast to 'Serokell.Util.colorize' which uses 'ANSI.Vivid' palete)
colorizeDull :: ANSI.Color -> Text -> Text
colorizeDull color msg =
    toText (ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Dull color]) <>
    msg <>
    toText (ANSI.setSGRCode [ANSI.Reset])

----------------------------------------------------------------------------
-- Aeson
----------------------------------------------------------------------------

-- | Parse a value represented as a 'show'-ed string in JSON.
parseJSONWithRead :: Read a => A.Value -> A.Parser a
parseJSONWithRead =
    either (fail . toString) pure . readEither @String <=<
    parseJSON
