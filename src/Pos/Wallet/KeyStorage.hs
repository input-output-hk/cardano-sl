{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Pos.Wallet.KeyStorage
       ( MonadKeys (..)
       , newSecretKey
       , KeyStorage
       , KeyData
       , KeyError (..)
       , runKeyStorage
       , runKeyStorageRaw
       ) where

import qualified Control.Concurrent.STM           as STM
import           Control.Lens                     (ix, lens, (%=), (.=), (<>=))
import           Control.Monad.Catch              (MonadThrow)
import qualified Control.Monad.Ether.Implicit     as Ether
import           Control.Monad.Reader             (ReaderT (..), ask)
import           Control.Monad.State              (MonadState (..))
import           Control.Monad.Trans              (MonadTrans (..))
import           Control.Monad.Trans.Ether.Tagged (TaggedTrans (..))
import qualified Control.Monad.Trans.Ether.Tagged as Ether
import           System.Wlog                      (WithLogger)
import           Universum

import           Pos.Binary.Crypto                ()
import           Pos.Context                      (NodeContext (..))
import           Pos.Context.Class                (getNodeContext)
import           Pos.Crypto                       (EncryptedSecretKey, PassPhrase,
                                                   SecretKey, hash, safeKeyGen)
import           Pos.Util                         ()
import           Pos.Util.Context                 (ContextTagK (..))
import           Pos.Util.UserSecret              (UserSecret, peekUserSecret, usKeys,
                                                   usPrimKey, writeUserSecret)
import           Pos.Util.Util                    (ether)

type KeyData = TVar UserSecret

----------------------------------------------------------------------
-- MonadKeys class
----------------------------------------------------------------------

class Monad m => MonadKeys m where
    getPrimaryKey   :: m (Maybe SecretKey)
    getSecretKeys   :: m [EncryptedSecretKey]
    addSecretKey    :: EncryptedSecretKey -> m ()
    modifySecretKey :: Word -> EncryptedSecretKey -> m ()
    deleteSecretKey :: Word -> m ()

    default getPrimaryKey :: (MonadTrans t, MonadKeys m', t m' ~ m) => m (Maybe SecretKey)
    getPrimaryKey = lift getPrimaryKey

    default getSecretKeys :: (MonadTrans t, MonadKeys m', t m' ~ m) => m [EncryptedSecretKey]
    getSecretKeys = lift getSecretKeys

    default addSecretKey :: (MonadTrans t, MonadKeys m', t m' ~ m) => EncryptedSecretKey -> m ()
    addSecretKey = lift . addSecretKey

    default modifySecretKey :: (MonadTrans t, MonadKeys m', t m' ~ m) => Word -> EncryptedSecretKey -> m ()
    modifySecretKey a = lift . modifySecretKey a

    default deleteSecretKey :: (MonadTrans t, MonadKeys m', t m' ~ m) => Word -> m ()
    deleteSecretKey = lift . deleteSecretKey

instance {-# OVERLAPPABLE #-}
    (MonadKeys m, MonadTrans t, Monad (t m)) =>
        MonadKeys (t m)

-- | Helper for generating a new secret key
newSecretKey :: (MonadIO m, MonadKeys m) => PassPhrase -> m EncryptedSecretKey
newSecretKey pp = do
    (_, sk) <- safeKeyGen pp
    addSecretKey sk
    return sk

------------------------------------------------------------------------
-- Common functions
------------------------------------------------------------------------

getSecret
    :: (MonadIO m, MonadReader KeyData m)
    => m UserSecret
getSecret = ask >>= atomically . STM.readTVar

putSecret
    :: (MonadIO m, MonadReader KeyData m)
    => UserSecret -> m ()
putSecret s = ask >>= atomically . flip STM.writeTVar s >> writeUserSecret s

deleteAt :: Int -> [a] -> [a]
deleteAt j ls = let (l, r) = splitAt j ls in l ++ drop 1 r

containsKey :: [EncryptedSecretKey] -> EncryptedSecretKey -> Bool
containsKey ls k = hash k `elem` map hash ls

------------------------------------------------------------------------
-- KeyStorage transformer
------------------------------------------------------------------------

type KeyStorage = Ether.ReaderT KeyData

runKeyStorage :: (MonadIO m, WithLogger m) => FilePath -> KeyStorage m a -> m a
runKeyStorage fp ks =
    peekUserSecret fp >>= liftIO . STM.newTVarIO >>= runKeyStorageRaw ks

runKeyStorageRaw :: KeyStorage m a -> KeyData -> m a
runKeyStorageRaw = Ether.runReaderT

instance {-# OVERLAPPING #-}
    (MonadIO m) =>
        MonadState UserSecret (KeyStorage m)
  where
    get = ether getSecret
    put = ether . putSecret

instance (MonadIO m) => MonadKeys (KeyStorage m) where
    getPrimaryKey = use usPrimKey
    getSecretKeys = use usKeys
    addSecretKey sk =
        whenM (not . (`containsKey` sk) <$> use usKeys) $
            usKeys <>= [sk]
    modifySecretKey (fromIntegral -> pos) newSK = usKeys . ix pos .= newSK
    deleteSecretKey (fromIntegral -> i) = usKeys %= deleteAt i

-------------------------------------------------------------------------
-- ContextHolder instance
-------------------------------------------------------------------------

data KeyError =
    PrimaryKey !Text -- ^ Failed attempt to delete primary key
    deriving (Show)

instance Exception KeyError

usLens :: Lens' (NodeContext ssc) KeyData
usLens = lens ncUserSecret $ \c us -> c { ncUserSecret = us }

instance {-# OVERLAPPING #-}
    (Monad m, t ~ ReaderT (NodeContext ssc)) =>
        MonadReader KeyData (TaggedTrans 'ContextTag t m)
  where
    ask = ncUserSecret <$> getNodeContext
    local f = Ether.pack . local (usLens %~ f) . Ether.unpack

instance {-# OVERLAPPING #-}
    (MonadIO m, t ~ ReaderT (NodeContext ssc)) =>
        MonadState UserSecret (TaggedTrans 'ContextTag t m)
  where
    get = getSecret
    put = putSecret

instance
    (MonadIO m, MonadThrow m, t ~ ReaderT (NodeContext ssc)) =>
        MonadKeys (TaggedTrans 'ContextTag t m)
  where
    getPrimaryKey = use usPrimKey
    getSecretKeys = use usKeys
    addSecretKey sk =
        whenM (not . (`containsKey` sk) <$> use usKeys) $
            usKeys <>= [sk]
    modifySecretKey (fromIntegral -> pos) newSK = usKeys . ix pos .= newSK
    deleteSecretKey (fromIntegral -> i) = usKeys %= deleteAt i
