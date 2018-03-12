{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Pos.Client.KeyStorage
       ( MonadKeysRead (..)
       , MonadKeys (..)
       , getSecretDefault
       , modifySecretPureDefault
       , modifySecretDefault

       , getPrimaryKey
       , getSecretKeys
       , getSecretKeysPlain
       , addSecretKey
       , deleteAllSecretKeys
       , deleteSecretKeyBy
       , newSecretKey
       , KeyData
       , KeyError (..)
       , AllUserSecrets (..)
       , keyDataFromFile
       ) where

import           Universum

import qualified Control.Concurrent.STM as STM
import           Control.Lens ((<%=), (<>~))
import           Serokell.Util (modifyTVarS)
import           System.Wlog (WithLogger)

import           Pos.Binary.Crypto ()
import           Pos.Crypto (EncryptedSecretKey, PassPhrase, SecretKey, hash, runSecureRandom,
                             safeKeyGen)
import           Pos.Util.UserSecret (HasUserSecret (..), UserSecret, peekUserSecret, usKeys,
                                      usPrimKey, writeUserSecret)

type KeyData = TVar UserSecret

----------------------------------------------------------------------
-- MonadKeys class and default functions
----------------------------------------------------------------------

class Monad m => MonadKeysRead m where
    getSecret :: m UserSecret

class MonadKeysRead m => MonadKeys m where
    modifySecret :: (UserSecret -> UserSecret) -> m ()

type HasKeysContext ctx m =
    ( MonadReader ctx m
    , HasUserSecret ctx
    , MonadIO m
    )

getSecretDefault :: HasKeysContext ctx m => m UserSecret
getSecretDefault = view userSecret >>= atomically . STM.readTVar

modifySecretPureDefault :: HasKeysContext ctx m => (UserSecret -> UserSecret) -> m ()
modifySecretPureDefault f = do
    us <- view userSecret
    void $ atomically $ modifyTVarS us (identity <%= f)

modifySecretDefault :: HasKeysContext ctx m => (UserSecret -> UserSecret) -> m ()
modifySecretDefault f = do
    us <- view userSecret
    new <- atomically $ modifyTVarS us (identity <%= f)
    writeUserSecret new

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

getPrimaryKey :: MonadKeysRead m => m (Maybe SecretKey)
getPrimaryKey = view usPrimKey <$> getSecret

newtype AllUserSecrets = AllUserSecrets
    { getAllUserSecrets :: [EncryptedSecretKey]
    } deriving (Container)

getSecretKeys :: MonadKeysRead m => m AllUserSecrets
getSecretKeys = AllUserSecrets . view usKeys <$> getSecret

getSecretKeysPlain :: MonadKeysRead m => m [EncryptedSecretKey]
getSecretKeysPlain = view usKeys <$> getSecret

addSecretKey :: MonadKeys m => EncryptedSecretKey -> m ()
addSecretKey sk = modifySecret $ \us ->
    if view usKeys us `containsKey` sk
    then us
    else us & usKeys <>~ [sk]

deleteAllSecretKeys :: MonadKeys m => m ()
deleteAllSecretKeys = modifySecret (usKeys .~ [])

deleteSecretKeyBy :: MonadKeys m => (EncryptedSecretKey -> Bool) -> m ()
deleteSecretKeyBy predicate = modifySecret (usKeys %~ filter (not . predicate))

-- | Helper for generating a new secret key
newSecretKey :: (MonadIO m, MonadKeys m) => PassPhrase -> m EncryptedSecretKey
newSecretKey pp = do
    (_, sk) <- liftIO $ runSecureRandom $ safeKeyGen pp
    addSecretKey sk
    pure sk

------------------------------------------------------------------------
-- Common functions
------------------------------------------------------------------------

containsKey :: [EncryptedSecretKey] -> EncryptedSecretKey -> Bool
containsKey ls k = hash k `elem` map hash ls

keyDataFromFile :: (MonadIO m, WithLogger m) => FilePath -> m KeyData
keyDataFromFile fp = peekUserSecret fp >>= liftIO . STM.newTVarIO

data KeyError =
    PrimaryKey !Text -- ^ Failed attempt to delete primary key
    deriving (Show)

instance Exception KeyError
