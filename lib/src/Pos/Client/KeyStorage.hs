{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Pos.Client.KeyStorage
       ( MonadKeys (..)
       , getPrimaryKey
       , getSecretKeys
       , getSecretKeysPlain
       , addSecretKey
       , deleteSecretKey
       , newSecretKey
       , KeyData
       , KeyError (..)
       , AllUserSecrets (..)
       , keyDataFromFile
       ) where

import           Universum

import qualified Control.Concurrent.STM as STM
import           Control.Lens           ((<>~))
import           System.Wlog            (WithLogger)

import           Pos.Binary.Crypto      ()
import           Pos.Crypto             (EncryptedSecretKey, PassPhrase, SecretKey, hash,
                                         runSecureRandom, safeKeyGen)
import           Pos.Util               ()
import           Pos.Util.UserSecret    (UserSecret, peekUserSecret, usKeys, usPrimKey)

type KeyData = TVar UserSecret

----------------------------------------------------------------------
-- MonadKeys class and abstract functions
----------------------------------------------------------------------

class Monad m => MonadKeys m where
    getSecret :: m UserSecret
    modifySecret :: (UserSecret -> UserSecret) -> m ()

getPrimaryKey :: MonadKeys m => m (Maybe SecretKey)
getPrimaryKey = view usPrimKey <$> getSecret

newtype AllUserSecrets = AllUserSecrets
    { getAllUserSecrets :: [EncryptedSecretKey]
    } deriving (Container, NontrivialContainer)

type instance Element AllUserSecrets = EncryptedSecretKey

getSecretKeys :: MonadKeys m => m AllUserSecrets
getSecretKeys = AllUserSecrets . view usKeys <$> getSecret

getSecretKeysPlain :: MonadKeys m => m [EncryptedSecretKey]
getSecretKeysPlain = view usKeys <$> getSecret

addSecretKey :: MonadKeys m => EncryptedSecretKey -> m ()
addSecretKey sk = modifySecret $ \us ->
    if view usKeys us `containsKey` sk
    then us
    else us & usKeys <>~ [sk]

deleteSecretKey :: MonadKeys m => Word -> m ()
deleteSecretKey (fromIntegral -> i) =
    modifySecret (usKeys %~ deleteAt i)

-- | Helper for generating a new secret key
newSecretKey :: (MonadIO m, MonadKeys m) => PassPhrase -> m EncryptedSecretKey
newSecretKey pp = do
    (_, sk) <- liftIO $ runSecureRandom $ safeKeyGen pp
    addSecretKey sk
    pure sk

------------------------------------------------------------------------
-- Common functions
------------------------------------------------------------------------

deleteAt :: Int -> [a] -> [a]
deleteAt j ls = let (l, r) = splitAt j ls in l ++ drop 1 r

containsKey :: [EncryptedSecretKey] -> EncryptedSecretKey -> Bool
containsKey ls k = hash k `elem` map hash ls

keyDataFromFile :: (MonadIO m, WithLogger m) => FilePath -> m KeyData
keyDataFromFile fp = peekUserSecret fp >>= liftIO . STM.newTVarIO

data KeyError =
    PrimaryKey !Text -- ^ Failed attempt to delete primary key
    deriving (Show)

instance Exception KeyError
