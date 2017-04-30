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
       , KSContext
       , runKSContext
       ) where

import qualified Control.Concurrent.STM as STM
import           Control.Lens           (lens, (%=), (<>=))
import           Control.Monad.Catch    (MonadThrow)
import           Control.Monad.Reader   (ask)
import           Control.Monad.State    (MonadState (..))
import           Control.Monad.Trans    (MonadTrans (..))
import           Data.Coerce
import qualified Ether
import           System.Wlog            (WithLogger)
import           Universum

import           Pos.Binary.Crypto      ()
import           Pos.Context            (NodeContext (..), NodeContextTag)
import           Pos.Context.Class      (WithNodeContext)
import           Pos.Crypto             (EncryptedSecretKey, PassPhrase, SecretKey, hash,
                                         safeKeyGen)
import           Pos.Util               ()
import           Pos.Util.UserSecret    (UserSecret, peekUserSecret, usKeys, usPrimKey,
                                         writeUserSecret)
import           Pos.Util.Util          (ether)

type KeyData = TVar UserSecret

----------------------------------------------------------------------
-- MonadKeys class
----------------------------------------------------------------------

-- FIXME: replace this with a MonadReader.
class Monad m => MonadKeys m where
    getPrimaryKey :: m (Maybe SecretKey)
    getSecretKeys :: m [EncryptedSecretKey]
    addSecretKey :: EncryptedSecretKey -> m ()
    deleteSecretKey :: Word -> m ()

    default getPrimaryKey :: (MonadTrans t, MonadKeys m', t m' ~ m) => m (Maybe SecretKey)
    getPrimaryKey = lift getPrimaryKey

    default getSecretKeys :: (MonadTrans t, MonadKeys m', t m' ~ m) => m [EncryptedSecretKey]
    getSecretKeys = lift getSecretKeys

    default addSecretKey :: (MonadTrans t, MonadKeys m', t m' ~ m) => EncryptedSecretKey -> m ()
    addSecretKey = lift . addSecretKey

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

type KeyStorage = Ether.ReaderT' KeyData

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

data KSContextTag

type KSContext = Ether.TaggedTrans KSContextTag Ether.IdentityT

runKSContext :: KSContext m a -> m a
runKSContext = coerce

instance {-# OVERLAPPING #-}
    (Monad m, t ~ Ether.IdentityT, WithNodeContext ssc m) =>
        MonadReader KeyData (Ether.TaggedTrans KSContextTag t m)
  where
    ask = Ether.asks @NodeContextTag ncUserSecret
    local f = Ether.local @NodeContextTag (over usLens f)

instance {-# OVERLAPPING #-}
    (MonadIO m, t ~ Ether.IdentityT, WithNodeContext ssc m) =>
        MonadState UserSecret (Ether.TaggedTrans KSContextTag t m)
  where
    get = getSecret
    put = putSecret

instance
    (MonadIO m, MonadThrow m, t ~ Ether.IdentityT, WithNodeContext ssc m) =>
        MonadKeys (Ether.TaggedTrans KSContextTag t m)
  where
    getPrimaryKey = use usPrimKey
    getSecretKeys = use usKeys
    addSecretKey sk =
        whenM (not . (`containsKey` sk) <$> use usKeys) $
            usKeys <>= [sk]
    deleteSecretKey (fromIntegral -> i) = usKeys %= deleteAt i
