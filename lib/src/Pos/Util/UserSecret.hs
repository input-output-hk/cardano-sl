{-# LANGUAGE CPP #-}

-- | Secret key file storage and management functions based on file
-- locking.

#if !defined(mingw32_HOST_OS)
#define POSIX
#endif

module Pos.Util.UserSecret
       ( WalletUserSecret (..)
       , wusRootKey
       , wusWalletName
       , wusAccounts
       , wusAddrs
       , accountGenesisIndex
       , wAddressGenesisIndex
       , mkGenesisWalletUserSecret

       , UserSecret
       , isEmptyUserSecret
       , usKeys
       , usVss
       , usWallet
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

import qualified Prelude
import           Universum hiding (keys)

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

#ifdef POSIX
import           Pos.Util.Trace.Named (TraceNamed, logInfo, logWarning)
#else
import           Pos.Util.Trace.Named (TraceNamed, logDebug, logInfo)
#endif

import           Pos.Binary.Class (Bi (..), Cons (..), Field (..), decodeFull',
                     deriveSimpleBi, encodeListLen, enforceSize, serialize')
import           Pos.Core (Address, accountGenesisIndex, addressF,
                     makeRootPubKeyAddress, wAddressGenesisIndex)
import           Pos.Crypto (EncryptedSecretKey, SecretKey, VssKeyPair,
                     encToPublic)
import           Pos.Util.UserKeyError (KeyError (..), UserKeyError (..),
                     UserKeyType (..))
import           Test.Pos.Crypto.Arbitrary ()

#ifdef POSIX
import           Formatting (oct, sformat)
import qualified System.Posix.Files as PSX
import qualified System.Posix.Types as PSX (FileMode)
#endif

-- Because of the Formatting import
{-# ANN module ("HLint: ignore Use fewer imports" :: Text) #-}

-- | Describes HD wallets keyfile content
data WalletUserSecret = WalletUserSecret
    { _wusRootKey    :: EncryptedSecretKey  -- ^ root key of wallet set
    , _wusWalletName :: Text                -- ^ name of wallet
    , _wusAccounts   :: [(Word32, Text)]    -- ^ accounts coordinates and names
    , _wusAddrs      :: [(Word32, Word32)]  -- ^ addresses coordinates
    } deriving (Show, Generic)

deriving instance Eq EncryptedSecretKey => Eq WalletUserSecret

makeLenses ''WalletUserSecret

instance Buildable WalletUserSecret where
    build WalletUserSecret{..} =
        bprint ("{ root = "%addressF%", set name = "%build%
                ", wallets = "%pairsF%", accounts = "%pairsF%" }")
        (makeRootPubKeyAddress $ encToPublic _wusRootKey)
        _wusWalletName
        _wusAccounts
        _wusAddrs
      where
        pairsF :: (Buildable a, Buildable b) => Format r ([(a, b)] -> r)
        pairsF = later $ mconcat . map (uncurry $ bprint ("("%build%", "%build%")"))

deriveSimpleBi ''WalletUserSecret [
    Cons 'WalletUserSecret [
        Field [| _wusRootKey    :: EncryptedSecretKey |],
        Field [| _wusWalletName :: Text               |],
        Field [| _wusAccounts   :: [(Word32, Text)]   |],
        Field [| _wusAddrs      :: [(Word32, Word32)] |]
    ]]

mkGenesisWalletUserSecret :: EncryptedSecretKey -> WalletUserSecret
mkGenesisWalletUserSecret _wusRootKey = do
    let _wusWalletName = "Genesis wallet"
        _wusAccounts   = [(accountGenesisIndex, "Genesis account")]
        _wusAddrs      = [(accountGenesisIndex, wAddressGenesisIndex)]
    WalletUserSecret{..}


-- | User secret data. Includes secret keys only for now (not
-- including auxiliary @_usPath@).
data UserSecret = UserSecret
    { _usKeys    :: [EncryptedSecretKey]
    , _usPrimKey :: Maybe SecretKey
    , _usVss     :: Maybe VssKeyPair
    , _usWallet  :: Maybe WalletUserSecret
    , _usPath    :: FilePath
    , _usLock    :: Maybe FileLock
    } deriving (Generic)

deriving instance Eq EncryptedSecretKey => Eq UserSecret

isEmptyUserSecret :: UserSecret -> Bool
isEmptyUserSecret us = null (_usKeys us)

makeLenses ''UserSecret

class HasUserSecret ctx where
    -- if you're going to mock this TVar, look how it's done for peer state.
    userSecret :: Lens' ctx (TVar UserSecret)

-- | Show instance to be able to include it into NodeParams
instance Bi Address => Show UserSecret where
    show UserSecret {..} =
        formatToString
            ("UserSecret { _usKeys = "%listJson%", _usVss = "%build%
             ", _usPath = "%build%", _usWallet = "%build%"}")
            _usKeys
            _usVss
            _usPath
            _usWallet

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

instance Bi UserSecret where
  encode us = encodeListLen 4 <> encode (_usVss us) <>
                                      encode (_usPrimKey us) <>
                                      encode (_usKeys us) <>
                                      encode (_usWallet us)
  decode = do
    enforceSize "UserSecret" 4
    vss  <- decode
    pkey <- decode
    keys <- decode
    wallet <- decode
    return $ def
        & usVss .~ vss
        & usPrimKey .~ pkey
        & usKeys .~ keys
        & usWallet .~ wallet

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

ensureModeIs600 :: MonadIO m => TraceNamed m -> FilePath -> m ()
#ifdef POSIX
ensureModeIs600 logTrace path = do
    accessMode <- getAccessMode path
    unless (accessMode == mode600) $ do
        logWarning logTrace $
            sformat ("Key file at "%build%" has access mode "%oct%" instead of 600. Fixing it automatically.")
            path accessMode
        setMode600 path
#else
ensureModeIs600 _ _ = do
    pure ()
#endif

-- | Create user secret file at the given path, but only when one doesn't
-- already exist.
initializeUserSecret :: MonadIO m => TraceNamed m -> FilePath -> m ()
initializeUserSecret logTrace secretPath = do
    exists <- liftIO $ doesFileExist secretPath
#ifdef POSIX
    if exists
    then ensureModeIs600 logTrace secretPath
    else do
        createEmptyFile secretPath
        setMode600 secretPath
#else
    logDebug logTrace "Windows: no permission checking on path"
    unless exists $ createEmptyFile secretPath
#endif
  where
    createEmptyFile :: (MonadIO m) => FilePath -> m ()
    createEmptyFile = liftIO . flip writeFile mempty

-- | Reads user secret from file, assuming that file exists,
-- and has mode 600, throws exception in other case
readUserSecret :: MonadIO m => TraceNamed m -> FilePath -> m UserSecret
readUserSecret logTrace path = do
#ifdef POSIX
    ensureModeIs600 logTrace path
#else
    logDebug logTrace "Windows: no permission checking on path"
#endif
    withReadLock path $ do
        content <- either (throwM . UserSecretDecodingError . toText) pure .
                   decodeFull' =<< BS.readFile path
        pure $ content & usPath .~ path

-- | Reads user secret from the given file.
-- If the file does not exist/is empty, returns empty user secret
peekUserSecret :: MonadIO m => TraceNamed m -> FilePath -> m UserSecret
peekUserSecret logTrace path = do
    logInfo logTrace "initalizing user secret"
    initializeUserSecret logTrace path
    withReadLock path $ do
        econtent <- decodeFull' <$> BS.readFile path
        pure $ either (const def) identity econtent & usPath .~ path

-- | Read user secret putting an exclusive lock on it. To unlock, use
-- 'writeUserSecretRelease'.
takeUserSecret :: MonadIO m => TraceNamed m -> FilePath -> m UserSecret
takeUserSecret logTrace path = do
    initializeUserSecret logTrace path
    liftIO $ do
        l <- lockFile (lockFilePath path) Exclusive
        econtent <- decodeFull' <$> BS.readFile path
        pure $ either (const def) identity econtent
            & usPath .~ path
            & usLock .~ Just l

-- | Writes user secret .
writeUserSecret :: MonadIO m => UserSecret -> m ()
writeUserSecret u
    | canWrite u = liftIO $ throwM $ KeyError Secret AlreadyLocked
    | otherwise = liftIO $ withFileLock (lockFilePath $ u ^. usPath) Exclusive $ const $ writeRaw u

-- | Writes user secret and releases the lock. UserSecret can't be
-- used after this function call anymore.
writeUserSecretRelease :: (MonadIO m, MonadThrow m) => UserSecret -> m ()
writeUserSecretRelease u
    | not (canWrite u) = throwM $ KeyError Secret NotWritable
    | otherwise = liftIO $ do
        writeRaw u
        case (u ^. usLock) of
            Nothing   -> throwM $ KeyError Secret IncorrectLock
            Just lock -> unlockFile lock

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

    BS.hPut tempHandle (serialize' u) `finally` hClose tempHandle

    renameFile tempPath path

-- | Helper for taking shared lock on file
withReadLock :: MonadIO m => FilePath -> IO a -> m a
withReadLock path = liftIO . withFileLock (lockFilePath path) Shared . const
