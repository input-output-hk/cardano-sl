{-# LANGUAGE CPP #-}

-- | Secret key file storage and management functions based on file
-- locking.

#if !defined(mingw32_HOST_OS)
#define POSIX
#endif

module Pos.Util.UserSecret
       ( UserSecret
       , usKeys
       , usVss
       , usWalletSet
       , usPrimKey
       , HasUserSecret(..)
       , getUSPath
       , simpleUserSecret
       , initializeUserSecret
       , readUserSecret
       , peekUserSecret
       , takeUserSecret
       , writeUserSecret
       , writeUserSecretRelease

       , UserSecretDecodingError (..)
       , ensureModeIs600
       ) where

import           Control.Exception     (onException)
import           Control.Lens          (makeLenses, to)
import qualified Data.ByteString       as BS
import           Data.Default          (Default (..))
import qualified Data.Text.Buildable
import           Formatting            (bprint, build, formatToString, (%))
import qualified Prelude
import           Serokell.Util.Text    (listJson)
import           System.FileLock       (FileLock, SharedExclusive (..), lockFile,
                                        unlockFile, withFileLock)
import qualified Turtle                as T
import           Universum

import           Pos.Binary.Class      (Bi (..), decodeFull, encode, label, labelS,
                                        putField)
import qualified Pos.Binary.Cbor       as Cbor
import           Pos.Binary.Crypto     ()
import           Pos.Crypto            (EncryptedSecretKey, SecretKey, VssKeyPair)

import           Pos.Types             (Address)
import           System.Directory      (renameFile)
import           System.FilePath       (takeDirectory, takeFileName)
import           System.IO             (hClose)
import           System.IO.Temp        (openBinaryTempFile)
import           System.Wlog           (WithLogger)

import           Pos.Wallet.Web.Secret (WalletUserSecret)

#ifdef POSIX
import           Formatting            (oct, sformat)
import qualified System.Posix.Files    as PSX
import qualified System.Posix.Types    as PSX (FileMode)
import           System.Wlog           (logWarning)
#endif

-- Because of the Formatting import
{-# ANN module ("HLint: ignore Use fewer imports" :: Text) #-}

-- | User secret data. Includes secret keys only for now (not
-- including auxiliary @_usPath@).
data UserSecret = UserSecret
    { _usKeys      :: [EncryptedSecretKey]
    , _usPrimKey   :: Maybe SecretKey
    , _usVss       :: Maybe VssKeyPair
    , _usWalletSet :: Maybe WalletUserSecret
    , _usPath      :: FilePath
    , _usLock      :: Maybe FileLock
    }

makeLenses ''UserSecret

class HasUserSecret ctx where
    -- if you're going to mock this TVar, look how it's done for peer state.
    userSecret :: Lens' ctx (TVar UserSecret)

-- | Show instance to be able to include it into NodeParams
instance Bi Address => Show UserSecret where
    show UserSecret {..} =
        formatToString
            ("UserSecret { _usKeys = "%listJson%", _usVss = "%build%
             ", _usPath = "%build%", _usWalletSet = "%build%"}")
            _usKeys
            _usVss
            _usPath
            _usWalletSet

newtype UserSecretDecodingError = UserSecretDecodingError Text
    deriving (Show)

instance Exception UserSecretDecodingError

instance Buildable UserSecretDecodingError where
    build (UserSecretDecodingError msg) =
        "Failed to decode user secret: " <> bprint build msg


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
    def = UserSecret [] Nothing Nothing Nothing "" Nothing

-- | It's not network/system-related, so instance shouldn't be under
-- @Pos.Binary.*@.
instance Bi UserSecret where
    sizeNPut = labelS "UserSecret" $
        putField _usVss <>
        putField _usPrimKey <>
        putField _usKeys <>
        putField _usWalletSet
    get = label "UserSecret" $ do
        vss <- get
        pkey <- get
        keys <- get
        wset <- get
        return $ def
            & usVss .~ vss
            & usPrimKey .~ pkey
            & usKeys .~ keys
            & usWalletSet .~ wset

instance Cbor.Bi UserSecret where
  encode us = Cbor.encodeListLen 4 <> Cbor.encode (_usVss us) <>
                                      Cbor.encode (_usPrimKey us) <>
                                      Cbor.encode (_usKeys us) <>
                                      Cbor.encode (_usWalletSet us)
  decode = do
    Cbor.enforceSize "UserSecret" 4
    vss  <- Cbor.decode
    pkey <- Cbor.decode
    keys <- Cbor.decode
    wset <- Cbor.decode
    return $ def
        & usVss .~ vss
        & usPrimKey .~ pkey
        & usKeys .~ keys
        & usWalletSet .~ wset

#ifdef POSIX
-- | Constant that defines file mode 600 (readable & writable only by owner).
mode600 :: PSX.FileMode
mode600 = PSX.unionFileModes PSX.ownerReadMode PSX.ownerWriteMode

-- | Return only the access part of the file mode (like owner:rw-, etc).
getAccessMode :: (MonadIO m) => FilePath -> m PSX.FileMode
getAccessMode path = do
    mode <- liftIO $ PSX.fileMode <$> PSX.getFileStatus path
    return $ PSX.intersectFileModes mode PSX.accessModes

-- | Set mode 600 on a given file, regardless of its current mode.
setMode600 :: (MonadIO m) => FilePath -> m ()
setMode600 path = liftIO $ PSX.setFileMode path mode600
#endif

ensureModeIs600 :: (MonadIO m, WithLogger m) => FilePath -> m ()
#ifdef POSIX
ensureModeIs600 path = do
    accessMode <- getAccessMode path
    unless (accessMode == mode600) $ do
        logWarning $
            sformat ("Key file at "%build%" has access mode "%oct%" instead of 600. Fixing it automatically.")
            path accessMode
        setMode600 path
#else
ensureModeIs600 _ = do
    pure ()
#endif

-- | Create user secret file at the given path, but only when one doesn't
-- already exist.
initializeUserSecret :: (MonadIO m, WithLogger m) => FilePath -> m ()
initializeUserSecret secretPath = do
    exists <- T.testfile (fromString secretPath)
#ifdef POSIX
    if exists
    then ensureModeIs600 secretPath
    else do
        createEmptyFile secretPath
        setMode600 secretPath
#else
    unless exists $ createEmptyFile secretPath
#endif
  where
    createEmptyFile :: (MonadIO m) => FilePath -> m ()
    createEmptyFile filePath = T.output (fromString filePath) empty

-- | Reads user secret from file, assuming that file exists,
-- and has mode 600, throws exception in other case
readUserSecret :: (MonadIO m, WithLogger m) => FilePath -> m UserSecret
readUserSecret path = do
#ifdef POSIX
    ensureModeIs600 path
#endif
    takeReadLock path $ do
        content <- either (throwM . UserSecretDecodingError . toText) pure .
                   decodeFull =<< BS.readFile path
        pure $ content & usPath .~ path

-- | Reads user secret from the given file.
-- If the file does not exist/is empty, returns empty user secret
peekUserSecret :: (MonadIO m, WithLogger m) => FilePath -> m UserSecret
peekUserSecret path = do
    initializeUserSecret path
    takeReadLock path $ do
        econtent <- decodeFull <$> BS.readFile path
        pure $ either (const def) identity econtent & usPath .~ path

-- | Read user secret putting an exclusive lock on it. To unlock, use
-- 'writeUserSecretRelease'.
takeUserSecret :: (MonadIO m, WithLogger m) => FilePath -> m UserSecret
takeUserSecret path = do
    initializeUserSecret path
    liftIO $ do
        l <- lockFile (lockFilePath path) Exclusive
        econtent <- decodeFull <$> BS.readFile path
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
    BS.hPut tempHandle (encode u) `onException` do
        hClose tempHandle

    hClose tempHandle
    renameFile tempPath path

-- | Helper for taking shared lock on file
takeReadLock :: MonadIO m => FilePath -> IO a -> m a
takeReadLock path = liftIO . withFileLock (lockFilePath path) Shared . const
