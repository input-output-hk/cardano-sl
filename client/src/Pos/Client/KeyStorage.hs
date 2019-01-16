{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Pos.Client.KeyStorage
       ( MonadKeysRead (..)
       , MonadKeys (..)
       , AllUserSecrets (..)
       , getSecretDefault
       , modifySecretPureDefault
       , modifySecretDefault

       , getSecretKeys
       , getSecretKeysPlain
       , addSecretKey
       , deleteAllSecretKeys
       , deleteSecretKeyBy
       ) where

import           Universum

import qualified Control.Concurrent.STM as STM
import           Control.Lens ((<%=), (<>~))
import           Serokell.Util (modifyTVarS)

import           Pos.Crypto (EncryptedSecretKey, hash)
import           Pos.Util.UserSecret (HasUserSecret (..), UserSecret, usKeys,
                     writeUserSecret)

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
    atomically $ STM.modifyTVar' us f

modifySecretDefault :: HasKeysContext ctx m => (UserSecret -> UserSecret) -> m ()
modifySecretDefault f = do
    us <- view userSecret
    new <- atomically $ modifyTVarS us (identity <%= f)
    writeUserSecret new

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

newtype AllUserSecrets = AllUserSecrets
    { getAllUserSecrets :: [EncryptedSecretKey]
    } deriving (Container)

getSecretKeys :: MonadKeysRead m => m AllUserSecrets
getSecretKeys = AllUserSecrets <$> getSecretKeysPlain

getSecretKeysPlain :: MonadKeysRead m => m [EncryptedSecretKey]
getSecretKeysPlain = view usKeys <$> getSecret

{-# INLINE addSecretKey #-}
addSecretKey :: MonadKeys m => EncryptedSecretKey -> m ()
addSecretKey sk = modifySecret $ \us ->
    if view usKeys us `containsKey` sk
    then us
    else us & usKeys <>~ [sk]
  where
    containsKey ls k = hash k `elem` map hash ls

deleteAllSecretKeys :: MonadKeys m => m ()
deleteAllSecretKeys = modifySecret (usKeys .~ [])

deleteSecretKeyBy :: MonadKeys m => (EncryptedSecretKey -> Bool) -> m ()
deleteSecretKeyBy predicate = modifySecret (usKeys %~ filter (not . predicate))
