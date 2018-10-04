{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Pos.Client.KeyStorage
       ( MonadKeysRead (..)
       , MonadKeys (..)
       , AllUserPublics (..)
       , AllUserSecrets (..)
       , getPublicDefault
       , getSecretDefault
       , modifyPublicPureDefault
       , modifySecretPureDefault
       , modifyPublicDefault
       , modifySecretDefault

       , getPublicKeys
       , getSecretKeys
       , getPublicKeysPlain
       , getSecretKeysPlain
       , addPublicKey
       , addSecretKey
       , deleteAllPublicKeys
       , deleteAllSecretKeys
       , deletePublicKeyBy
       , deleteSecretKeyBy
       ) where

import           Universum

import qualified Control.Concurrent.STM as STM
import           Control.Lens ((<%=), (<>~))
import           Serokell.Util (modifyTVarS)

import           Pos.Crypto (EncryptedSecretKey, PublicKey, hash)
import           Pos.Util.UserPublic (HasUserPublic (..), UserPublic, upKeys,
                     writeUserPublic)
import           Pos.Util.UserSecret (HasUserSecret (..), UserSecret, usKeys,
                     writeUserSecret)

----------------------------------------------------------------------
-- MonadKeys class and default functions
----------------------------------------------------------------------

class Monad m => MonadKeysRead m where
    getPublic :: m UserPublic
    getSecret :: m UserSecret

class MonadKeysRead m => MonadKeys m where
    modifyPublic :: (UserPublic -> UserPublic) -> m ()
    modifySecret :: (UserSecret -> UserSecret) -> m ()

type HasKeysContext ctx m =
    ( MonadReader ctx m
    , HasUserPublic ctx
    , HasUserSecret ctx
    , MonadIO m
    )

getPublicDefault :: HasKeysContext ctx m => m UserPublic
getPublicDefault = view userPublic >>= atomically . STM.readTVar

getSecretDefault :: HasKeysContext ctx m => m UserSecret
getSecretDefault = view userSecret >>= atomically . STM.readTVar

modifyPublicPureDefault :: HasKeysContext ctx m => (UserPublic -> UserPublic) -> m ()
modifyPublicPureDefault f = do
    up <- view userPublic
    atomically $ STM.modifyTVar' up f

modifySecretPureDefault :: HasKeysContext ctx m => (UserSecret -> UserSecret) -> m ()
modifySecretPureDefault f = do
    us <- view userSecret
    atomically $ STM.modifyTVar' us f

modifyPublicDefault :: HasKeysContext ctx m => (UserPublic -> UserPublic) -> m ()
modifyPublicDefault f = do
    up <- view userPublic
    new <- atomically $ modifyTVarS up (identity <%= f)
    writeUserPublic new

modifySecretDefault :: HasKeysContext ctx m => (UserSecret -> UserSecret) -> m ()
modifySecretDefault f = do
    us <- view userSecret
    new <- atomically $ modifyTVarS us (identity <%= f)
    writeUserSecret new

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

newtype AllUserPublics = AllUserPublics
    { getAllUserPublics :: [PublicKey]
    } deriving (Container)

newtype AllUserSecrets = AllUserSecrets
    { getAllUserSecrets :: [EncryptedSecretKey]
    } deriving (Container)

getPublicKeys :: MonadKeysRead m => m AllUserPublics
getPublicKeys = AllUserPublics <$> getPublicKeysPlain

getSecretKeys :: MonadKeysRead m => m AllUserSecrets
getSecretKeys = AllUserSecrets <$> getSecretKeysPlain

getPublicKeysPlain :: MonadKeysRead m => m [PublicKey]
getPublicKeysPlain = view upKeys <$> getPublic

getSecretKeysPlain :: MonadKeysRead m => m [EncryptedSecretKey]
getSecretKeysPlain = view usKeys <$> getSecret

addPublicKey :: MonadKeys m => PublicKey -> m ()
addPublicKey pk = modifyPublic $ \up ->
    if view upKeys up `containsPublicKey` pk
    then up
    else up & upKeys <>~ [pk]
  where
    containsPublicKey = flip elem

addSecretKey :: MonadKeys m => EncryptedSecretKey -> m ()
addSecretKey sk = modifySecret $ \us ->
    if view usKeys us `containsKey` sk
    then us
    else us & usKeys <>~ [sk]
  where
    containsKey ls k = hash k `elem` map hash ls

deleteAllPublicKeys :: MonadKeys m => m ()
deleteAllPublicKeys = modifyPublic (upKeys .~ [])

deleteAllSecretKeys :: MonadKeys m => m ()
deleteAllSecretKeys = modifySecret (usKeys .~ [])

deletePublicKeyBy :: MonadKeys m => (PublicKey -> Bool) -> m ()
deletePublicKeyBy predicate = modifyPublic (upKeys %~ filter (not . predicate))

deleteSecretKeyBy :: MonadKeys m => (EncryptedSecretKey -> Bool) -> m ()
deleteSecretKeyBy predicate = modifySecret (usKeys %~ filter (not . predicate))
