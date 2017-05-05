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
import           Control.Lens           ((<>~))
import           Control.Monad.Catch    (MonadThrow)
import qualified Ether
import           System.Wlog            (WithLogger)
import           Universum

import           Pos.Binary.Crypto      ()
import           Pos.Crypto             (EncryptedSecretKey, PassPhrase, SecretKey, hash,
                                         safeKeyGen)
import           Pos.Util               ()
import           Pos.Util.UserSecret    (UserSecret, peekUserSecret, usKeys, usPrimKey,
                                         writeUserSecret)

type KeyData = TVar UserSecret

----------------------------------------------------------------------
-- MonadKeys class
----------------------------------------------------------------------

type MonadKeys m =
    ( Ether.MonadReader' KeyData m
    , MonadIO m
    , MonadThrow m )

getPrimaryKey :: MonadKeys m => m (Maybe SecretKey)
getPrimaryKey = view usPrimKey <$> getSecret

getSecretKeys :: MonadKeys m => m [EncryptedSecretKey]
getSecretKeys = view usKeys <$> getSecret

addSecretKey :: MonadKeys m => EncryptedSecretKey -> m ()
addSecretKey sk = do
    us <- getSecret
    unless (view usKeys us `containsKey` sk) $
        putSecret (us & usKeys <>~ [sk])

deleteSecretKey :: MonadKeys m => Word -> m ()
deleteSecretKey (fromIntegral -> i) =
    modifySecret (usKeys %~ deleteAt i)

-- | Helper for generating a new secret key
newSecretKey :: MonadKeys m => PassPhrase -> m EncryptedSecretKey
newSecretKey pp = do
    (_, sk) <- safeKeyGen pp
    addSecretKey sk
    return sk

------------------------------------------------------------------------
-- Common functions
------------------------------------------------------------------------

getSecret :: MonadKeys m => m UserSecret
getSecret = Ether.ask' >>= atomically . STM.readTVar

putSecret :: MonadKeys m => UserSecret -> m ()
putSecret s = Ether.ask' >>= atomically . flip STM.writeTVar s >> writeUserSecret s

modifySecret :: MonadKeys m => (UserSecret -> UserSecret) -> m ()
modifySecret f =
    -- TODO: Current definition preserves the behavior before the refactoring.
    -- It can be improved if we access the TVar just once to modify it instead
    -- of reading and writing it separately.
    putSecret . f =<< getSecret

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
