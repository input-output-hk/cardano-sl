{-# LANGUAGE TemplateHaskell #-}

-- | Secret key file storage and management functions based on file
-- locking.

module Pos.Util.UserSecret
       ( UserSecret
       , usKeys
       , usVss
       , getUSPath
       , simpleUserSecret
       , peekUserSecret
       , takeUserSecret
       , writeUserSecret
       , writeUserSecretRelease
       ) where

import           Control.Lens         (makeLenses, to)
import           Data.Binary.Get      (label)
import qualified Data.ByteString.Lazy as BSL
import           Data.Default         (Default (..))
import           Prelude              (show)
import           System.FileLock      (FileLock, SharedExclusive (..), lockFile,
                                       unlockFile, withFileLock)
import qualified Turtle               as T
import           Universum            hiding (show)

import           Pos.Binary.Class     (Bi (..), decodeFull, encode)
import           Pos.Crypto           (SecretKey, VssKeyPair)

-- | User secret data. Includes secret keys only for now (not
-- including auxiliary @_usPath@).
data UserSecret = UserSecret
    { _usKeys :: [SecretKey]
    , _usVss  :: Maybe VssKeyPair
    , _usPath :: FilePath
    , _usLock :: Maybe FileLock
    }

makeLenses ''UserSecret

-- | Show instance to be able to include it into NodeParams
instance Show UserSecret where
    show UserSecret {..} =
        "UserSecret { _usKeys = " ++ show _usKeys ++
        ", _usVss = " ++ show _usVss ++
        ", _usPath = " ++ show _usPath

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
simpleUserSecret sk fp = def & usKeys .~ [sk] & usPath .~ fp

instance Default UserSecret where
    def = UserSecret [] Nothing "" Nothing

-- | It's not network/system-related, so instance shouldn't be under
-- @Pos.Binary.*@.
instance Bi UserSecret where
    put UserSecret{..} = put _usVss >> put _usKeys
    get = label "UserSecret" $ do
        vss <- get
        keys <- get
        return $ def & usVss .~ vss & usKeys .~ keys

-- | Create user secret file at the given path, but only when one doesn't
-- already exist.
initializeSecret :: (MonadIO m) => FilePath -> m ()
initializeSecret path = do
    exists <- T.testfile (fromString path)
    liftIO (if exists
            then return ()
            else T.output (fromString path) empty)

-- | Reads user secret from the given file.
-- If the file does not exist/is empty, returns empty user secret
peekUserSecret :: (MonadIO m) => FilePath -> m UserSecret
peekUserSecret path =
    liftIO $ withFileLock (lockFilePath path) Shared $ const $ do
        initializeSecret path
        econtent <- decodeFull <$> BSL.readFile path
        pure $ either (const def) identity econtent & usPath .~ path

-- | Read user secret putting an exclusive lock on it. To unlock, use
-- 'writeUserSecretRelease'.
takeUserSecret :: (MonadIO m) => FilePath -> m UserSecret
takeUserSecret path = liftIO $ do
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
            (fromMaybe (panic "writeUserSecretRelease: incorrect UserSecret") $
            u ^. usLock)

-- | Helper for writing secret to file
writeRaw :: UserSecret -> IO ()
writeRaw u = BSL.writeFile (u ^. usPath) $ encode u
