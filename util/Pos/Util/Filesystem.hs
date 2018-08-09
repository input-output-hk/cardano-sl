-- | Filesystem-related utilities.

module Pos.Util.Filesystem
       (
         ls
       , lstree
       , withTempDir
       , directory
       , withTempFile
       , withSystemTempFile
       , withMaybeFile
       ) where

import           Universum hiding (last)

import           Control.Concurrent (myThreadId)
import qualified Control.Exception.Safe as E
import           Data.Char (isAlphaNum)
import           Data.List (last)
import           Data.Time (getCurrentTime)
import qualified Prelude
import           System.Directory (canonicalizePath, createDirectory, doesDirectoryExist,
                                   getTemporaryDirectory, listDirectory, removeDirectoryRecursive,
                                   removeFile)
import           System.FilePath (normalise, pathSeparator, takeDirectory, (</>))
import           System.IO (hClose, openTempFile)

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
withSystemTempFile :: (MonadIO m, E.MonadMask m) =>
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
  E.bracket
    (liftIO (openTempFile tmpDir template))
    (\(name, handle) -> liftIO (hClose handle >> ignoringIOErrors (removeFile name)))
    (uncurry action)
  where
     ignoringIOErrors :: E.MonadCatch m => m () -> m ()
     ignoringIOErrors ioe = ioe `E.catch` (\e -> const (return ()) (e :: Prelude.IOError))

withMaybeFile :: (MonadIO m, MonadMask m) => Maybe FilePath -> IOMode -> (Maybe Handle -> m r) -> m r
withMaybeFile Nothing     _    f = f Nothing
withMaybeFile (Just file) mode f =
    bracket (openFile file mode) (liftIO . hClose) (f . Just)
