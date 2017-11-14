{-# LANGUAGE CPP             #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE RankNTypes      #-}

module Pos.Util.Util
       (
       -- * Something
         Sign (..)
       , maybeThrow
       , eitherToFail
       , eitherToThrow
       , getKeys
       , sortWithMDesc
       , leftToPanic
       , dumpSplices
       , histogram
       , median
       , (<//>)
       , divRoundUp

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

       ) where

import           Universum

import           Control.Concurrent (myThreadId, threadDelay)
import           Control.Lens (Getting, Iso', coerced, foldMapOf, ( # ))
import qualified Control.Monad.Catch as MC
import           Control.Monad.Trans.Class (MonadTrans)
import           Data.Aeson (FromJSON (..))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import           Data.Char (isAlphaNum)
import           Data.HashSet (fromMap)
import           Data.List (last)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Semigroup as Smg
import           Data.Time (getCurrentTime)
import           Data.Time.Clock (NominalDiffTime)
import qualified Ether
import           Ether.Internal (HasLens (..))
import qualified Language.Haskell.TH as TH
import qualified Prelude
import qualified System.Console.ANSI as ANSI
import           System.Directory (canonicalizePath, createDirectory, doesDirectoryExist,
                                   getTemporaryDirectory, listDirectory, removeDirectoryRecursive,
                                   removeFile)
import           System.FilePath (normalise, pathSeparator, takeDirectory, (</>))
import           System.IO (hClose, openTempFile)

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
withSystemTempFile template action = do
    tmpDir <- liftIO getCanonicalTemporaryDirectory
    withTempFile tmpDir template action

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
