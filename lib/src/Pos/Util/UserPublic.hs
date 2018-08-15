{-# LANGUAGE CPP #-}

-- | Public key file storage and management functions based on file
-- locking.

#if !defined(mingw32_HOST_OS)
#define POSIX
#endif

module Pos.Util.UserPublic
       ( WalletUserPublic (..)
       , wupWalletName
       , wupAccounts
       , wupAddrs

       , UserPublic
       , upKeys
       , upWallet
       , HasUserPublic(..)
       , initializeUserPublic
       , peekUserPublic
       , takeUserPublic
       , writeUserPublic
       , writeUserPublicRelease
       ) where

import qualified Prelude
import           Universum

import           Control.Exception.Safe (finally)
import           Control.Lens (makeLenses, to)
import qualified Data.ByteString as BS
import           Data.Default (Default (..))
import           Formatting (Format, bprint, build, formatToString, later, (%))
import qualified Formatting.Buildable
import           Serokell.Util.Text (listJson)
import           System.Directory (doesFileExist)
import           System.Directory (renameFile)
import           System.FileLock (FileLock, SharedExclusive (..), lockFile,
                     unlockFile, withFileLock)
import           System.FilePath (takeDirectory, takeFileName)
import           System.IO (hClose, openBinaryTempFile)

import           Pos.Binary.Class (Bi (..), Cons (..), Field (..), decodeFull',
                     deriveSimpleBi, encodeListLen, enforceSize, serialize')
import           Pos.Crypto (PublicKey)
import           Pos.Util.UserKeyError (KeyError (..), UserKeyError (..),
                     UserKeyType (..))
#ifdef POSIX
import           Pos.Util.Trace.Named (TraceNamed, logWarning)
#else
import           Pos.Util.Trace.Named (TraceNamed, logDebug)
#endif

import           Test.Pos.Crypto.Arbitrary ()

#ifdef POSIX
import           Formatting (oct, sformat)
import qualified System.Posix.Files as PSX
import qualified System.Posix.Types as PSX (FileMode)
#endif

-- Because of the Formatting import
{-# ANN module ("HLint: ignore Use fewer imports" :: Text) #-}

-- | Describes HD wallets keyfile content
data WalletUserPublic = WalletUserPublic
    { _wupWalletName :: Text                -- ^ name of wallet
    , _wupAccounts   :: [(Word32, Text)]    -- ^ accounts coordinates and names
    , _wupAddrs      :: [(Word32, Word32)]  -- ^ addresses coordinates
    } deriving (Eq, Generic, Show)

makeLenses ''WalletUserPublic

instance Buildable WalletUserPublic where
    build WalletUserPublic{..} =
        bprint ("{ wallet name = "%build%
                ", accounts = "%pairsF%", addresses = "%pairsF%" }")
        _wupWalletName
        _wupAccounts
        _wupAddrs
      where
        pairsF :: (Buildable a, Buildable b) => Format r ([(a, b)] -> r)
        pairsF = later $ mconcat . map (uncurry $ bprint ("("%build%", "%build%")"))

deriveSimpleBi ''WalletUserPublic [
    Cons 'WalletUserPublic [
        Field [| _wupWalletName :: Text               |],
        Field [| _wupAccounts   :: [(Word32, Text)]   |],
        Field [| _wupAddrs      :: [(Word32, Word32)] |]
    ]]

-- | User public data.
data UserPublic = UserPublic
    { _upKeys   :: [PublicKey]
    , _upWallet :: Maybe WalletUserPublic
    , _upPath   :: FilePath
    , _upLock   :: Maybe FileLock
    } deriving (Eq, Generic)

makeLenses ''UserPublic

class HasUserPublic ctx where
    -- if you're going to mock this TVar, look how it's done for peer state.
    userPublic :: Lens' ctx (TVar UserPublic)

-- | Show instance to be able to include it into NodeParams
instance Show UserPublic where
    show UserPublic {..} =
        formatToString
            ("UserPublic { _upKeys = "%listJson%
             ", _upPath = "%build%", _upWallet = "%build%"}")
            _upKeys
            _upPath
            _upWallet

-- | Path of lock file for the provided path.
lockFilePath :: FilePath -> FilePath
lockFilePath = (<> ".lock")

-- | Checks if this user public instance can be dumped back to
-- file. If not, using 'writeUserPublic' and 'writeUserPublicRelease'
-- will result in error.
canWrite :: UserPublic -> Bool
canWrite up = up ^. upLock . to isJust

instance Default UserPublic where
    def = UserPublic [] Nothing "" Nothing

instance Bi UserPublic where
  encode us = encodeListLen 2 <> encode (_upKeys us)
                              <> encode (_upWallet us)
  decode = do
    enforceSize "UserPublic" 2
    pKeys  <- decode
    wallet <- decode
    return $ def
        & upKeys   .~ pKeys
        & upWallet .~ wallet

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

ensureModeIs600 :: MonadIO m => TraceNamed m -> FilePath -> m ()
ensureModeIs600 logTrace path = do
    accessMode <- getAccessMode path
    unless (accessMode == mode600) $ do
        logWarning logTrace $
            sformat ("Key file at "%build%" has access mode "%oct%" instead of 600. Fixing it automatically.")
            path accessMode
        setMode600 path
#endif

-- | Create user public file at the given path, but only when one doesn't
-- already exist.
initializeUserPublic :: MonadIO m => TraceNamed m -> FilePath -> m ()
initializeUserPublic logTrace publicPath = do
    exists <- liftIO $ doesFileExist publicPath
#ifdef POSIX
    if exists
    then ensureModeIs600 logTrace publicPath
    else do
        createEmptyFile publicPath
        setMode600 publicPath
#else
    logDebug logTrace "Windows: no permission checking on path"
    unless exists $ createEmptyFile publicPath
#endif
  where
    createEmptyFile :: (MonadIO m) => FilePath -> m ()
    createEmptyFile = liftIO . flip writeFile mempty

-- | Reads user public from the given file.
-- If the file does not exist/is empty, returns empty user public
peekUserPublic :: MonadIO m => TraceNamed m -> FilePath -> m UserPublic
peekUserPublic logTrace path = do
    initializeUserPublic logTrace path
    withReadLock path $ do
        content <- decodeFull' <$> BS.readFile path
        pure $ either (const def) identity content & upPath .~ path

-- | Read user public putting an exclusive lock on it. To unlock, use
-- 'writeUserPublicRelease'.
takeUserPublic :: MonadIO m => TraceNamed m -> FilePath -> m UserPublic
takeUserPublic logTrace path = do
    initializeUserPublic logTrace path
    liftIO $ do
        lock <- lockFile (lockFilePath path) Exclusive
        content <- decodeFull' <$> BS.readFile path
        pure $ either (const def) identity content
            & upPath .~ path
            & upLock .~ Just lock

-- | Writes user public.
writeUserPublic :: (MonadIO m) => UserPublic -> m ()
writeUserPublic up
    | canWrite up = liftIO $ throwM $ KeyError Public AlreadyLocked
    | otherwise   = liftIO $ withFileLock (lockFilePath $ up ^. upPath) Exclusive $ const $ writeRaw up

-- | Writes user public and releases the lock. UserPublic can't be
-- used after this function call anymore.
writeUserPublicRelease :: (MonadIO m, MonadThrow m) => UserPublic -> m ()
writeUserPublicRelease up
    | not (canWrite up) = throwM $ KeyError Public NotWritable
    | otherwise = liftIO $ do
        writeRaw up
        case (up ^. upLock) of
            Nothing   -> throwM $ KeyError Public IncorrectLock
            Just lock -> unlockFile lock

-- | Helper for writing public to file
writeRaw :: UserPublic -> IO ()
writeRaw up = do
    let path = up ^. upPath
    -- On POSIX platforms, openBinaryTempFile guarantees that the file
    -- will be created with mode 600.
    -- If openBinaryTempFile throws, we want to propagate this exception,
    -- hence no handler.
    (tempPath, tempHandle) <-
        openBinaryTempFile (takeDirectory path) (takeFileName path)

    BS.hPut tempHandle (serialize' up) `finally` hClose tempHandle

    renameFile tempPath path

-- | Helper for taking shared lock on file
withReadLock :: MonadIO m => FilePath -> IO a -> m a
withReadLock path = liftIO . withFileLock (lockFilePath path) Shared . const
