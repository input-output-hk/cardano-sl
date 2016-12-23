{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Pos.Wallet.KeyStorage
       ( MonadKeys (..)
       , newSecretKey
       , KeyStorage (..)
       , KeyData
       , KeyError (..)
       , runKeyStorage
       , runKeyStorageRaw
       ) where

import qualified Control.Concurrent.STM      as STM
import           Control.Lens                (Lens', iso, lens, use, (%=), (%~), (<>=))
import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Catch         (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Reader        (ReaderT (..), ask)
import           Control.Monad.State         (MonadState (..))
import           Control.Monad.Trans         (MonadTrans (..))
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..), StM,
                                              defaultLiftBaseWith, defaultLiftWith,
                                              defaultRestoreM, defaultRestoreT)
import           Control.TimeWarp.Rpc        (MonadDialog, MonadTransfer (..))
import           Control.TimeWarp.Timed      (MonadTimed (..), ThreadId)
import           Serokell.Util.Lens          (WrappedM (..))
import           System.Wlog                 (CanLog, HasLoggerName)
import           Universum

import           Pos.Context                 (ContextHolder (..), NodeContext (..),
                                              WithNodeContext (..))
import           Pos.Crypto                  (SecretKey, keyGen)
import qualified Pos.DB                      as Modern
import           Pos.DHT.Model               (MonadDHT, MonadMessageDHT,
                                              WithDefaultMsgHeader)
import           Pos.DHT.Real                (KademliaDHT)
import           Pos.Slotting                (MonadSlots)
import qualified Pos.State                   as St
import           Pos.Txp.LocalData           (MonadTxLD)
import           Pos.Util                    ()
import           Pos.Util.UserSecret         (UserSecret, peekUserSecret, usKeys,
                                              writeUserSecret)

import           Pos.Wallet.Context          (WithWalletContext)
import           Pos.Wallet.State.State      (MonadWalletDB)

type KeyData = STM.TVar UserSecret

----------------------------------------------------------------------
-- MonadKeys class
----------------------------------------------------------------------

class Monad m => MonadKeys m where
    getSecretKeys :: m [SecretKey]
    addSecretKey :: SecretKey -> m ()
    deleteSecretKey :: Word -> m ()

    default getSecretKeys :: MonadTrans t => t m [SecretKey]
    getSecretKeys = lift getSecretKeys

    default addSecretKey :: MonadTrans t => SecretKey -> t m ()
    addSecretKey = lift . addSecretKey

    default deleteSecretKey :: MonadTrans t => Word -> t m ()
    deleteSecretKey = lift . deleteSecretKey

-- | Instances for common transformers
instance MonadKeys m => MonadKeys (ReaderT r m)
instance MonadKeys m => MonadKeys (StateT s m)

-- | Instances for ancestor in the monadic stack
instance MonadKeys m => MonadKeys (KademliaDHT m)

-- | Helper for generating a new secret key
newSecretKey :: (MonadIO m, MonadKeys m) => m SecretKey
newSecretKey = do
    (_, sk) <- keyGen
    addSecretKey sk
    return sk

------------------------------------------------------------------------
-- Common functions
------------------------------------------------------------------------

getSecret :: (MonadIO m, MonadReader KeyData m) => m UserSecret
getSecret = ask >>= atomically . STM.readTVar

putSecret :: (MonadIO m, MonadReader KeyData m) => UserSecret -> m ()
putSecret s = ask >>= atomically . flip STM.writeTVar s >> writeUserSecret s

deleteAt :: Int -> [a] -> [a]
deleteAt j ls = let (l, r) = splitAt j ls in l ++ drop 1 r

------------------------------------------------------------------------
-- KeyStorage transformer
------------------------------------------------------------------------

newtype KeyStorage m a = KeyStorage
    { getKeyStorage :: ReaderT KeyData m a
    } deriving (Functor, Applicative, Monad, MonadTimed,
                MonadThrow, MonadSlots, MonadCatch, MonadIO,
                HasLoggerName, MonadDialog s p, CanLog, MonadMask, MonadDHT,
                MonadMessageDHT s, MonadReader KeyData, WithDefaultMsgHeader,
                MonadWalletDB, WithWalletContext, MonadTxLD, WithNodeContext ssc,
                St.MonadDB ssc, Modern.MonadDB ssc)

type instance ThreadId (KeyStorage m) = ThreadId m

instance Monad m => WrappedM (KeyStorage m) where
    type UnwrappedM (KeyStorage m) = ReaderT KeyData m
    _WrappedM = iso getKeyStorage KeyStorage

instance MonadTransfer s m => MonadTransfer s (KeyStorage m)

instance MonadTrans KeyStorage where
    lift = KeyStorage . lift

instance MonadIO m => MonadState UserSecret (KeyStorage m) where
    get = KeyStorage getSecret
    put = KeyStorage . putSecret

instance MonadBase IO m => MonadBase IO (KeyStorage m) where
    liftBase = lift . liftBase

instance MonadTransControl KeyStorage where
    type StT KeyStorage a = StT (ReaderT KeyData) a
    liftWith = defaultLiftWith KeyStorage getKeyStorage
    restoreT = defaultRestoreT KeyStorage

instance MonadBaseControl IO m => MonadBaseControl IO (KeyStorage m) where
    type StM (KeyStorage m) a = ComposeSt KeyStorage m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM

runKeyStorage :: MonadIO m => FilePath -> KeyStorage m a -> m a
runKeyStorage fp ks =
    peekUserSecret fp >>= liftIO . STM.newTVarIO >>= runKeyStorageRaw ks

runKeyStorageRaw :: KeyStorage m a -> KeyData -> m a
runKeyStorageRaw = runReaderT . getKeyStorage

instance MonadIO m => MonadKeys (KeyStorage m) where
    getSecretKeys = use usKeys
    addSecretKey sk = usKeys <>= [sk]
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

instance Monad m => MonadReader KeyData (ContextHolder ssc m) where
    ask = ncUserSecret <$> getNodeContext
    local f = ContextHolder . local (usLens %~ f) . getContextHolder

instance MonadIO m => MonadState UserSecret (ContextHolder ssc m) where
    get = getSecret
    put = putSecret

instance (MonadIO m, MonadThrow m) => MonadKeys (ContextHolder ssc m) where
    getSecretKeys = use usKeys
    addSecretKey sk = usKeys <>= [sk]
    deleteSecretKey (fromIntegral -> i)
        | i == 0 = throwM $ PrimaryKey "Cannot delete a primary secret key"
        | otherwise = usKeys %= deleteAt i
