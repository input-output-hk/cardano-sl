{-# LANGUAGE TemplateHaskell #-}

-- | Secret key file storage and management functions based on file
-- locking.

module Pos.Util.UserSecret
       ( UserSecret
       , usKeys
       , peekUserSecret
       , takeUserSecret
       , writeUserSecret
       , writeUserSecretRelease
       ) where

import           Control.Lens         (makeLenses, to, (.~), (^.))
import           Control.Monad.Fail   (MonadFail, fail)
import qualified Data.ByteString.Lazy as BSL
import           System.FileLock      (FileLock, SharedExclusive (..), lockFile,
                                       unlockFile, withFileLock)
import           Universum

import           Pos.Binary           (Bi (..), decode, encode)
import           Pos.Crypto           (SecretKey)

-- | User secret data. Includes secret keys only for now (not
-- including auxiliary @_usPath@).
data UserSecret = UserSecret
    { _usKeys :: [SecretKey]
    , _usPath :: FilePath
    , _usLock :: Maybe FileLock
    }

makeLenses ''UserSecret

-- | Checks if this user secret instance can be dumped back to
-- file. If not, using 'writeUserSecret' and 'writeUserSecretRelease'
-- will result in error.
canWrite :: UserSecret -> Bool
canWrite u = u ^. usLock . to isJust

-- | It's not network/system-related, so instance shouldn't be under
-- @Pos.Binary.*@.
instance Bi UserSecret where
    put UserSecret{..} = put _usKeys
    get = do
        _usKeys <- get
        pure $ UserSecret { _usPath = "", _usLock = Nothing, ..}

-- | Reads user secret from the given file.
peekUserSecret :: (MonadIO m) => FilePath -> m UserSecret
peekUserSecret path =
    liftIO $ withFileLock path Shared $ const $ do
        content <- decode <$> BSL.readFile path
        pure $ content & usPath .~ path

-- | Read user secret putting an exclusive lock on it. To unlock, use
-- 'writeUserSecret' or 'writeUserSecretRelease'.
takeUserSecret :: (MonadIO m) => FilePath -> m UserSecret
takeUserSecret path = liftIO $ do
    l <- lockFile path Exclusive
    content <- decode <$> BSL.readFile path
    pure $ content & usPath .~ path
                   & usLock .~ Just l

-- | Writes user secret if file is locked by this process.
writeUserSecret :: (MonadIO m, MonadFail m) => UserSecret -> m ()
writeUserSecret u
    | not (canWrite u) = fail "writeUserSecret: UserSecret is not writable"
    | otherwise = liftIO $ BSL.writeFile (u ^. usPath) $ encode u

-- | Writes user secret and releases the lock. UserSecret can't be
-- used after this function call anymore.
writeUserSecretRelease :: (MonadIO m, MonadFail m) => UserSecret -> m ()
writeUserSecretRelease u
    | not (canWrite u) = fail "writeUserSecret: UserSecret is not writable"
    | otherwise = do
        writeUserSecret u
        liftIO $ unlockFile
            (fromMaybe (panic "writeUserSecretRelease: incorrect UserSecret") $
            u ^. usLock)
