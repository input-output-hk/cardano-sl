{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Pos.Client.KeyStorage
       ( MonadKeysRead (..)
       , MonadKeys (..)
       , AllUserSecrets (..)
       , AllUserPublics (..)
       , getSecretDefault
       , getPublicDefault
       , modifySecretPureDefault
       , modifyPublicPureDefault
       , modifySecretDefault
       , modifyPublicDefault
       , getSecretKeys
       , getPublicKeys
       , getSecretKeysPlain
       , getPublicKeysPlain
       , addSecretKey
       , addPublicKey
       , deleteAllSecretKeys
       , deleteAllPublicKeys
       , deleteSecretKeyBy
       , deletePublicKeyBy
       ) where

import           Universum

import qualified Control.Concurrent.STM as STM
import           Control.Lens ((<%=), (<>~))
import           Serokell.Util (modifyTVarS)

import           Pos.Crypto (EncryptedSecretKey, PublicKey, hash)
import           Pos.Util.UserSecret (HasUserSecret (..), UserSecret, usKeys,
                     writeUserSecret)
import           Pos.Util.UserPublic (HasUserPublic (..), UserPublic, upKeys,
                     writeUserPublic)

----------------------------------------------------------------------
-- MonadKeys class and default functions
----------------------------------------------------------------------

class Monad m => MonadKeysRead m where
    getSecret :: m UserSecret
    getPublic :: m UserPublic

class MonadKeysRead m => MonadKeys m where
    modifySecret :: (UserSecret -> UserSecret) -> m ()
    modifyPublic :: (UserPublic -> UserPublic) -> m ()

type HasKeysContext ctx m =
    ( MonadReader ctx m
    , HasUserSecret ctx
    , HasUserPublic ctx
    , MonadIO m
    )

getSecretDefault :: HasKeysContext ctx m => m UserSecret
getSecretDefault = view userSecret >>= atomically . STM.readTVar

getPublicDefault :: HasKeysContext ctx m => m UserPublic
getPublicDefault = view userPublic >>= atomically . STM.readTVar

modifySecretPureDefault :: HasKeysContext ctx m => (UserSecret -> UserSecret) -> m ()
modifySecretPureDefault f = do
    us <- view userSecret
    atomically $ STM.modifyTVar' us f

modifyPublicPureDefault :: HasKeysContext ctx m => (UserPublic -> UserPublic) -> m ()
modifyPublicPureDefault f = do
    up <- view userPublic
    atomically $ STM.modifyTVar' up f

modifySecretDefault :: HasKeysContext ctx m => (UserSecret -> UserSecret) -> m ()
modifySecretDefault f = do
    us <- view userSecret
    new <- atomically $ modifyTVarS us (identity <%= f)
    writeUserSecret new

modifyPublicDefault :: HasKeysContext ctx m => (UserPublic -> UserPublic) -> m ()
modifyPublicDefault f = do
    up <- view userPublic
    new <- atomically $ modifyTVarS up (identity <%= f)
    writeUserPublic new

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

newtype AllUserSecrets = AllUserSecrets
    { getAllUserSecrets :: [EncryptedSecretKey]
    } deriving (Container)

newtype AllUserPublics = AllUserPublics
    { getAllUserPublics :: [PublicKey]
    } deriving (Container)

-- type instance Element AllUserSecrets = EncryptedSecretKey
-- type instance Element AllUserPublics = PublicKey

getSecretKeys :: MonadKeysRead m => m AllUserSecrets
getSecretKeys = AllUserSecrets <$> getSecretKeysPlain

getPublicKeys :: MonadKeysRead m => m AllUserPublics
getPublicKeys = AllUserPublics <$> getPublicKeysPlain

getSecretKeysPlain :: MonadKeysRead m => m [EncryptedSecretKey]
getSecretKeysPlain = view usKeys <$> getSecret

getPublicKeysPlain :: MonadKeysRead m => m [PublicKey]
getPublicKeysPlain = view upKeys <$> getPublic

addSecretKey :: MonadKeys m => EncryptedSecretKey -> m ()
addSecretKey sk = modifySecret $ \us ->
    if view usKeys us `containsKey` sk
    then us
    else us & usKeys <>~ [sk]
  where
    containsKey ls k = hash k `elem` map hash ls

addPublicKey :: MonadKeys m => PublicKey -> m ()
addPublicKey pk = modifyPublic $ \up ->
    if view upKeys up `containsPublicKey` pk
    then up
    else up & upKeys <>~ [pk]
  where
    containsPublicKey = flip elem

deleteAllSecretKeys :: MonadKeys m => m ()
deleteAllSecretKeys = modifySecret (usKeys .~ [])

deleteAllPublicKeys :: MonadKeys m => m ()
deleteAllPublicKeys = modifyPublic (upKeys .~ [])

deleteSecretKeyBy :: MonadKeys m => (EncryptedSecretKey -> Bool) -> m ()
deleteSecretKeyBy predicate = modifySecret (usKeys %~ filter (not . predicate))

deletePublicKeyBy :: MonadKeys m => (PublicKey -> Bool) -> m ()
deletePublicKeyBy predicate = modifyPublic (upKeys %~ filter (not . predicate))
