{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Secret key file storage and management functions based on file
-- locking.

#if !defined(mingw32_HOST_OS)
#define POSIX
#endif

module Pos.Util.UserSecret
       ( UserSecret
       , usKeys
       , usVss
       , usPrimKey
       , getUSPath
       , simpleUserSecret
       , initializeUserSecret
       , readUserSecret
       , peekUserSecret
       , takeUserSecret
       , writeUserSecret
       , writeUserSecretRelease
       ) where

import           Control.Exception    (onException)
import           Control.Lens         (makeLenses, to)
import           Data.Binary.Get      (label)
import qualified Data.ByteString.Lazy as BSL
import           Data.Default         (Default (..))
import           Formatting           (build, formatToString, (%))
import qualified Prelude
import           Serokell.Util.Text   (listJson)
import           System.FileLock      (FileLock, SharedExclusive (..), lockFile,
                                       unlockFile, withFileLock)
import qualified Turtle               as T
import           Universum

import           Pos.Binary.Class     (Bi (..), decodeFull, encode)
import           Pos.Binary.Crypto    ()
import           Pos.Crypto           (EncryptedSecretKey, SecretKey, VssKeyPair)

import           System.Directory     (renameFile)
import           System.FilePath      (takeDirectory, takeFileName)
import           System.IO            (hClose)
import           System.IO.Temp       (openBinaryTempFile)

import           Pos.Wallet.Web.Error (WalletError (..))

#ifdef POSIX
import           Formatting           (oct, sformat)
import qualified System.Posix.Files   as PSX
import qualified System.Posix.Types   as PSX (FileMode)
#endif

-- | User secret data. Includes secret keys only for now (not
-- including auxiliary @_usPath@).
data UserSecret = UserSecret
    { _usKeys    :: [EncryptedSecretKey]
    , _usPrimKey :: Maybe SecretKey
    , _usVss     :: Maybe VssKeyPair
    , _usPath    :: FilePath
    , _usLock    :: Maybe FileLock
    }

makeLenses ''UserSecret

-- | Show instance to be able to include it into NodeParams
instance Show UserSecret where
    show UserSecret {..} =
        formatToString ("UserSecret { _usKeys = "%listJson%", _usVss = "%build%", _usPath = "%build%"}")
            _usKeys _usVss _usPath

-- | Path of lock file for the provided path.
lockFilePath :: FilePath -> FilePath
lockFilePath = (<> ".lock")

-- | Checks if this user secret instance can be dumped back to
-- file. If not, using 'writeUserSecret' and 'writeUserSecretRelease'
-- will result in error.
canWrite :: UserSecret -> Bool
canWrite u = u ^. usLock . to isJust

getUSPath :: UserSecret -> FilePath
getUSPath = flip (^.) usPath

-- | Create a simple UserSecret from secret key and file path
simpleUserSecret :: SecretKey -> FilePath -> UserSecret
simpleUserSecret sk fp = def & usPrimKey .~ Just sk & usPath .~ fp

instance Default UserSecret where
    def = UserSecret [] Nothing Nothing "" Nothing

-- | It's not network/system-related, so instance shouldn't be under
-- @Pos.Binary.*@.
instance Bi UserSecret where
    put UserSecret{..} = put _usVss >> put _usPrimKey >> put _usKeys
    get = label "UserSecret" $ do
        vss <- get
        pkey <- get
        keys <- get
        return $ def & usVss .~ vss & usPrimKey .~ pkey & usKeys .~ keys

#ifdef POSIX
-- | Constant that defines file mode 600 (readable & writable only by owner).
mode600 :: PSX.FileMode
mode600 = PSX.unionFileModes PSX.ownerReadMode PSX.ownerWriteMode

-- | Check whether a given file has mode 600.
failIfModeNot600 :: (MonadIO m, MonadThrow m) => FilePath -> m ()
failIfModeNot600 path = do
    mode <- liftIO $ PSX.fileMode <$> PSX.getFileStatus path
    let accessMode = PSX.intersectFileModes mode PSX.accessModes
    when (accessMode /= mode600) $
        throwM $ Internal $
            sformat ("Key file access mode is incorrect. Set it to 600 and try again. Key file path: "%build%" Current mode: "%oct)
            path accessMode

-- | Set mode 600 on a given file, regardless of its current mode.
setMode600 :: (MonadIO m) => FilePath -> m ()
setMode600 path = liftIO $ PSX.setFileMode path mode600
#endif

-- | Create user secret file at the given path, but only when one doesn't
-- already exist.
initializeUserSecret :: (MonadIO m, MonadThrow m) => FilePath -> m ()
initializeUserSecret secretPath = do
    exists <- T.testfile (fromString secretPath)
    liftIO $
#ifdef POSIX
        if exists
        then failIfModeNot600 secretPath
        else do
            createEmptyFile secretPath
            setMode600 secretPath
#else
        when (not exists) $
            createEmptyFile secretPath
#endif
  where
    createEmptyFile :: (MonadIO m) => FilePath -> m ()
    createEmptyFile filePath = T.output (fromString filePath) empty

-- | Reads user secret from file, assuming that file exists,
-- and has mode 600, throws exception in other case
readUserSecret :: (MonadIO m, MonadThrow m) => FilePath -> m UserSecret
readUserSecret path = takeReadLock path $ do
#ifdef POSIX
    failIfModeNot600 path
#endif
    content <- either (throwM . Internal . toText) pure . decodeFull =<< BSL.readFile path
    pure $ content & usPath .~ path

-- | Reads user secret from the given file.
-- If the file does not exist/is empty, returns empty user secret
peekUserSecret :: (MonadIO m, MonadThrow m) => FilePath -> m UserSecret
peekUserSecret path = takeReadLock path $ do
    initializeUserSecret path
    econtent <- decodeFull <$> BSL.readFile path
    pure $ either (const def) identity econtent & usPath .~ path

-- | Read user secret putting an exclusive lock on it. To unlock, use
-- 'writeUserSecretRelease'.
takeUserSecret :: (MonadIO m, MonadThrow m) => FilePath -> m UserSecret
takeUserSecret path = liftIO $ do
    initializeUserSecret path
    l <- lockFile (lockFilePath path) Exclusive
    econtent <- decodeFull <$> BSL.readFile path
    pure $ either (const def) identity econtent
        & usPath .~ path
        & usLock .~ Just l

-- | Writes user secret .
writeUserSecret :: (MonadIO m) => UserSecret -> m ()
writeUserSecret u
    | canWrite u = liftIO $ fail "writeUserSecret: UserSecret is already locked"
    | otherwise = liftIO $ withFileLock (lockFilePath $ u ^. usPath) Exclusive $ const $ writeRaw u

-- | Writes user secret and releases the lock. UserSecret can't be
-- used after this function call anymore.
writeUserSecretRelease :: (MonadFail m, MonadIO m) => UserSecret -> m ()
writeUserSecretRelease u
    | not (canWrite u) = fail "writeUserSecretRelease: UserSecret is not writable"
    | otherwise = liftIO $ do
          writeRaw u
          unlockFile
            (fromMaybe (error "writeUserSecretRelease: incorrect UserSecret") $
            u ^. usLock)

-- | Helper for writing secret to file
writeRaw :: UserSecret -> IO ()
writeRaw u = do
    let path = u ^. usPath
    -- On POSIX platforms, openBinaryTempFile guarantees that the file
    -- will be created with mode 600.
    -- If openBinaryTempFile throws, we want to propagate this exception,
    -- hence no handler.
    (tempPath, tempHandle) <-
        openBinaryTempFile (takeDirectory path) (takeFileName path)

    -- onException rethrows the exception after calling the handler.
    BSL.hPut tempHandle (encode u) `onException` do
        hClose tempHandle

    hClose tempHandle
    renameFile tempPath path

-- | Helper for taking shared lock on file
takeReadLock :: MonadIO m => FilePath -> IO a -> m a
takeReadLock path = liftIO . withFileLock (lockFilePath path) Shared . const
