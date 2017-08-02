{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Miscellaneous unclassified utility functions.

module Pos.Util
       (
         module Pos.Util.Util
       -- * Stuff for testing and benchmarking
       , module Pos.Util.Arbitrary
       , module Pos.Util.TimeLimit
       -- * Concurrency helpers
       , module Pos.Util.Concurrent

       -- * Various
       , mappendPair
       , mconcatPair
       , (<//>)
       , eitherToVerRes
       , readerToState
       , diffDoubleMap
       , withMaybeFile
       , mapEither
       , microsecondsToUTC

       -- * NonEmpty
       , neZipWith3
       , neZipWith4
       , spanSafe

       -- * Filesystem utilities
       , ls
       , lstree
       , withTempDir
       , directory

       -- * Instances
       -- ** MonadFail ParsecT
       -- ** MonadFail Dialog
       -- ** MonadFail Transfer
       -- ** MonadFail TimedIO
       -- ** MonadFail ResponseT
       -- ** MonadFail LoggerNameBox
       ) where

import           Universum                    hiding (finally)

import           Control.Concurrent           (myThreadId)
import qualified Control.Monad                as Monad (fail)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Either                  (rights)
import           Data.Hashable                (Hashable)
import qualified Data.HashMap.Strict          as HM
import           Data.List                    (last, span, zipWith3, zipWith4)
import           Data.Ratio                   ((%))
import qualified Data.Text                    as T
import           Data.Time.Clock              (UTCTime)
import           Data.Time.Clock.POSIX        (posixSecondsToUTCTime)
import           Data.Time.Units              (Microsecond, toMicroseconds)
import           Serokell.Util                (VerificationRes (..))
import           System.Directory             (canonicalizePath, createDirectory,
                                               doesDirectoryExist, listDirectory,
                                               removeDirectory)
import           System.FilePath              (normalise, takeDirectory, (</>))
import           System.IO                    (hClose)
import           System.Wlog                  (LoggerNameBox (..))
import           Text.Parsec                  (ParsecT)
-- SafeCopy instance for HashMap
import           Serokell.AcidState           ()

import           Pos.Util.Arbitrary
import           Pos.Util.Concurrent
import           Pos.Util.TimeLimit
import           Pos.Util.Undefined           ()
import           Pos.Util.Util

-- | Specialized version of 'mappend' for restricted to pair type.
mappendPair :: (Monoid a, Monoid b) => (a, b) -> (a, b) -> (a, b)
mappendPair = mappend

-- | Specialized version of 'mconcat' (or 'Data.Foldable.fold')
-- for restricting type to list of pairs.
mconcatPair :: (Monoid a, Monoid b) => [(a, b)] -> (a, b)
mconcatPair = mconcat

-- | Concatenates two url part using regular slash '/'.
-- E.g. @"./dir/" <//> "/file" = "./dir/file"@.
(<//>) :: String -> String -> String
(<//>) lhs rhs = lhs' ++ "/" ++ rhs'
  where
    isSlash = (== '/')
    lhs' = reverse $ dropWhile isSlash $ reverse lhs
    rhs' = dropWhile isSlash rhs

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

withMaybeFile :: (MonadIO m, MonadMask m) => Maybe FilePath -> IOMode -> (Maybe Handle -> m r) -> m r
withMaybeFile Nothing     _    f = f Nothing
withMaybeFile (Just file) mode f =
    bracket (openFile file mode) (liftIO . hClose) (f . Just)

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


instance MonadFail (ParsecT s u m) where
    fail = Monad.fail

deriving instance MonadFail m => MonadFail (LoggerNameBox m)

instance MonadFail m => MonadFail (ResourceT m) where
    fail = lift . fail

----------------------------------------------------------------------------
-- Filesystem utilities
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
        tid <- myThreadId
        pth <- canonicalizePath $ normalise $ parentDir </> (toString template <> show tid)
        createDirectory pth
        return pth

    dispose :: FilePath -> IO ()
    dispose = removeDirectory

-- | Simple shim to emulate the behaviour of `Filesystem.Path.directory`,
-- which is a bit more lenient than `System.FilePath.takeDirectory`.
directory :: FilePath -> FilePath
directory "" = ""
directory f = case last f of
    '/' -> f
    _   -> takeDirectory (normalise f)
