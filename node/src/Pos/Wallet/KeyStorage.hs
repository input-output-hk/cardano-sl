{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Pos.Wallet.KeyStorage
       ( MonadKeys
       , getPrimaryKey
       , getSecretKeys
--        , addSecretKey
--        , deleteSecretKey
--        , newSecretKey
       , KeyData
       , KeyError (..)
       , keyDataFromFile
       ) where

import qualified Control.Concurrent.STM as STM
import           Control.Lens           ((<>~), mapped)
import           Control.Monad.Catch    (MonadThrow)
import           System.Wlog            (WithLogger)
import           Universum

import           Pos.Binary.Crypto      ()
import           Pos.Crypto             (EncryptedSecretKey, PassPhrase, SecretKey, hash,
                                         safeKeyGen)
import           Pos.Util               ()
import           Pos.Util.UserSecret    (HasUserSecret (..), UserSecret, peekUserSecret,
                                         usWallets, usPrimKey, writeUserSecret)
import           Pos.Wallet.Web.Secret  (wusRootKey)

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
getSecretKeys = over mapped (view wusRootKey) . view usWallets <$> getSecret

deleteWalletSet :: MonadKeys ctx m => Word -> m ()
deleteWalletSet (fromIntegral -> i) =
    modifySecret (usWallets %~ deleteAt i)

-- AJ: Restore similar functionality.
-- addSecretKey :: MonadKeys ctx m => EncryptedSecretKey -> m ()
-- addSecretKey sk = do
--     us <- getSecret
--     unless (fmap (view wusRootKey) (view usWallets us) `containsKey` sk) $
--         putSecret (us & usKeys <>~ [sk])
--
-- deleteSecretKey :: MonadKeys ctx m => Word -> m ()
-- deleteSecretKey (fromIntegral -> i) =
--     modifySecret (usKeys %~ deleteAt i)
--
-- -- | Helper for generating a new secret key
-- newSecretKey :: MonadKeys ctx m => PassPhrase -> m EncryptedSecretKey
-- newSecretKey pp = do
--     (_, sk) <- safeKeyGen pp
--     addSecretKey sk
--     return sk

------------------------------------------------------------------------
-- Common functions
------------------------------------------------------------------------

getSecret :: MonadKeys ctx m => m UserSecret
getSecret = view userSecret >>= atomically . STM.readTVar

putSecret :: MonadKeys ctx m => UserSecret -> m ()
putSecret s = view userSecret >>= atomically . flip STM.writeTVar s >> writeUserSecret s

modifySecret :: MonadKeys ctx m => (UserSecret -> UserSecret) -> m ()
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
