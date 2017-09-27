{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Pos.Wallet.KeyStorage
       ( MonadKeys
       , getPrimaryKey
       , getSecretKeys
       , addSecretKey
       , deleteSecretKey
       , newSecretKey
       , KeyData
       , KeyError (..)
       , keyDataFromFile
       ) where

import qualified Control.Concurrent.STM as STM
import           Control.Lens           ((<%=), (<>~))
import           Control.Monad.Catch    (MonadThrow)
import           Serokell.Util          (modifyTVarS)
import           System.Wlog            (WithLogger)
import           Universum

import           Pos.Binary.Crypto      ()
import           Pos.Crypto             (EncryptedSecretKey, PassPhrase, SecretKey, hash,
                                         runSecureRandom, safeKeyGen)
import           Pos.Util               ()
import           Pos.Util.UserSecret    (HasUserSecret (..), UserSecret, peekUserSecret,
                                         usKeys, usPrimKey, writeUserSecret)

type KeyData = TVar UserSecret

----------------------------------------------------------------------
-- MonadKeys class
----------------------------------------------------------------------

type MonadKeys ctx m =
    ( MonadReader ctx m
    , HasUserSecret ctx
    , MonadIO m
    , MonadThrow m )

getPrimaryKey :: MonadKeys ctx m => m (Maybe SecretKey)
getPrimaryKey = view usPrimKey <$> getSecret

getSecretKeys :: MonadKeys ctx m => m [EncryptedSecretKey]
getSecretKeys = view usKeys <$> getSecret

addSecretKey :: MonadKeys ctx m => EncryptedSecretKey -> m ()
addSecretKey sk = do
    modifySecret $ \us ->
        if view usKeys us `containsKey` sk
        then us
        else us & usKeys <>~ [sk]

deleteSecretKey :: MonadKeys ctx m => Word -> m ()
deleteSecretKey (fromIntegral -> i) =
    modifySecret (usKeys %~ deleteAt i)

-- | Helper for generating a new secret key
newSecretKey :: MonadKeys ctx m => PassPhrase -> m EncryptedSecretKey
newSecretKey pp = do
    (_, sk) <- liftIO $ runSecureRandom $ safeKeyGen pp
    addSecretKey sk
    return sk

------------------------------------------------------------------------
-- Common functions
------------------------------------------------------------------------

getSecret :: MonadKeys ctx m => m UserSecret
getSecret = view userSecret >>= atomically . STM.readTVar

modifySecret
    :: MonadKeys ctx m
    => (UserSecret -> UserSecret) -> m ()
modifySecret f = do
    us <- view userSecret
    new <- atomically $ modifyTVarS us (identity <%= f)
    writeUserSecret new

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
