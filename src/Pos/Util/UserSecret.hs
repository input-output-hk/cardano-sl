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
import           Control.Monad        (fail)
import qualified Data.ByteString.Lazy as BSL
import           Data.Default         (Default (..))
import           System.FileLock      (FileLock, SharedExclusive (..), lockFile,
                                       unlockFile, withFileLock)
import           Universum

import           Pos.Binary           (Bi (..), decode, decodeFull, encode)
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

instance Default UserSecret where
    def = UserSecret [] "" Nothing

-- | It's not network/system-related, so instance shouldn't be under
-- @Pos.Binary.*@.
instance Bi UserSecret where
    put UserSecret{..} = put _usKeys
    get = get >>= pure . flip (usKeys .~) def

-- | Reads user secret from the given file.
-- If the file does not exist/is empty, returns empty user secret
peekUserSecret :: (MonadIO m) => FilePath -> m UserSecret
peekUserSecret path =
    liftIO $ withFileLock path Shared $ const $ do
        econtent <- decodeFull <$> BSL.readFile path
        pure $ either (const def) identity econtent & usPath .~ path

-- | Read user secret putting an exclusive lock on it. To unlock, use
-- 'writeUserSecret' or 'writeUserSecretRelease'.
takeUserSecret :: (MonadIO m) => FilePath -> m UserSecret
takeUserSecret path = liftIO $ do
    l <- lockFile path Exclusive
    content <- decode <$> BSL.readFile path
    pure $ content & usPath .~ path
                   & usLock .~ Just l

-- | Writes user secret .
writeUserSecret :: (MonadIO m) => UserSecret -> m ()
writeUserSecret u
    | canWrite u = fail "writeUserSecret: UserSecret is already locked"
    | otherwise = liftIO $ withFileLock (u ^. usPath) Exclusive $ const $
                  BSL.writeFile (u ^. usPath) $ encode u

-- | Writes user secret and releases the lock. UserSecret can't be
-- used after this function call anymore.
writeUserSecretRelease :: (MonadIO m) => UserSecret -> m ()
writeUserSecretRelease u
    | not (canWrite u) = fail "writeUserSecretRelease: UserSecret is not writable"
    | otherwise = do
        writeUserSecret u
        liftIO $ unlockFile
            (fromMaybe (panic "writeUserSecretRelease: incorrect UserSecret") $
            u ^. usLock)
