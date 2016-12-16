{-# LANGUAGE CPP                   #-}
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

import qualified Control.Concurrent.STM as STM
import           Control.Lens           (iso, use, (%=), (<>=))
import           Control.Monad.Catch    (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Reader   (ReaderT (..), ask)
import           Control.Monad.State    (MonadState (..))
import           Control.Monad.Trans    (MonadTrans (..))
import           Control.TimeWarp.Rpc   (MonadDialog, MonadTransfer (..))
import           Control.TimeWarp.Timed (MonadTimed (..), ThreadId)
import           Serokell.Util.Lens     (WrappedM (..))
import           System.Wlog            (CanLog, HasLoggerName)
import           Universum

import           Pos.Context            (WithNodeContext)
import           Pos.Crypto             (SecretKey, keyGen)
import           Pos.Txp.LocalData      (MonadTxLD)
#ifdef WITH_ROCKS
import qualified Pos.Modern.DB          as Modern (MonadDB)
import qualified Pos.Modern.Txp.Class   as Modern (MonadTxpLD)
#endif
import           Pos.DHT.Model          (MonadDHT, MonadMessageDHT, WithDefaultMsgHeader)
import           Pos.Slotting           (MonadSlots)
import           Pos.Ssc.Class          (MonadSscLD)
import           Pos.State              (MonadDB)
import           Pos.Util               ()
import           Pos.Util.JsonLog       (MonadJL)
import           Pos.Util.UserSecret    (UserSecret, peekUserSecret, usKeys,
                                         writeUserSecret)

-- | Typeclass of monad with access to secret keys
class Monad m => MonadKeys m where
    getSecretKeys :: m [SecretKey]
    addSecretKey :: SecretKey -> m ()
    deleteSecretKey :: Word -> m ()

-- | Instances for common transformers
instance MonadKeys m => MonadKeys (ReaderT r m) where
    getSecretKeys = lift getSecretKeys
    addSecretKey = lift . addSecretKey
    deleteSecretKey = lift . deleteSecretKey

instance MonadKeys m => MonadKeys (StateT s m) where
    getSecretKeys = lift getSecretKeys
    addSecretKey = lift . addSecretKey
    deleteSecretKey = lift . deleteSecretKey

-- | Helper for generating a new secret key
newSecretKey :: (MonadIO m, MonadKeys m) => m SecretKey
newSecretKey = do
    (_, sk) <- keyGen
    addSecretKey sk
    return sk

type KeyData = STM.TVar UserSecret

newtype KeyStorage m a = KeyStorage
    { getKeyStorage :: ReaderT KeyData m a
    } deriving (Functor, Applicative, Monad, MonadTimed, MonadTxLD,
                MonadThrow, MonadSlots, MonadCatch, MonadIO,
                HasLoggerName, MonadDialog s p, WithNodeContext ssc,
                MonadJL, MonadDB ssc, CanLog, MonadMask, MonadDHT,
                MonadMessageDHT s, MonadSscLD ssc, MonadReader KeyData,
                WithDefaultMsgHeader)

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

#ifdef WITH_ROCKS
deriving instance Modern.MonadDB ssc m => Modern.MonadDB ssc (KeyStorage m)
deriving instance Modern.MonadTxpLD ssc m => Modern.MonadTxpLD ssc (KeyStorage m)
#endif

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
