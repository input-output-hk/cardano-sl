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
       , runKeyStorage
       , runKeyStorageRaw
       ) where

import qualified Control.Concurrent.STM      as STM
import           Control.Lens                (iso, use, (%=), (<>=))
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

import           Pos.Crypto                  (SecretKey, keyGen)
import           Pos.DHT.Model               (MonadDHT, MonadMessageDHT,
                                              WithDefaultMsgHeader)
import           Pos.DHT.Real                (KademliaDHT)
import           Pos.Slotting                (MonadSlots)
import           Pos.Util                    ()
import           Pos.Util.UserSecret         (UserSecret, peekUserSecret, usKeys,
                                              writeUserSecret)

import           Pos.Wallet.Context          (WithWalletContext)
import           Pos.Wallet.State.State      (MonadWalletDB)

-- | Typeclass of monad with access to secret keys
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

type KeyData = STM.TVar UserSecret

newtype KeyStorage m a = KeyStorage
    { getKeyStorage :: ReaderT KeyData m a
    } deriving (Functor, Applicative, Monad, MonadTimed,
                MonadThrow, MonadSlots, MonadCatch, MonadIO,
                HasLoggerName, MonadDialog s p, CanLog, MonadMask, MonadDHT,
                MonadMessageDHT s, MonadReader KeyData, WithDefaultMsgHeader,
                MonadWalletDB, WithWalletContext)

type instance ThreadId (KeyStorage m) = ThreadId m

instance Monad m => WrappedM (KeyStorage m) where
    type UnwrappedM (KeyStorage m) = ReaderT KeyData m
    _WrappedM = iso getKeyStorage KeyStorage

instance MonadTransfer s m => MonadTransfer s (KeyStorage m)

instance MonadTrans KeyStorage where
    lift = KeyStorage . lift

instance MonadIO m => MonadState UserSecret (KeyStorage m) where
    get = KeyStorage ask >>= atomically . STM.readTVar
    put s = KeyStorage ask >>= atomically . flip STM.writeTVar s >>
            writeUserSecret s

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
      where deleteAt j ls = let (l, r) = splitAt j ls
                            in l ++ drop 1 r
