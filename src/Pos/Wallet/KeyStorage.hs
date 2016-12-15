{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Pos.Wallet.KeyStorage
       (
       ) where

import qualified Control.Concurrent.STM    as STM
import           Control.Lens              (iso)
import           Control.Monad.Catch       (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Reader      (ReaderT (ReaderT), ask)
import           Control.Monad.Trans.Class (MonadTrans)
import           Control.TimeWarp.Rpc      (MonadDialog, MonadTransfer (..))
import           Control.TimeWarp.Timed    (MonadTimed (..), ThreadId)
import           Serokell.Util.Lens        (WrappedM (..))
import           System.Wlog               (CanLog, HasLoggerName)
import           Universum

import           Pos.Context               (WithNodeContext)
#ifdef WITH_ROCKS
import qualified Pos.Modern.DB             as Modern (MonadDB)
import qualified Pos.Modern.Txp.Class      as Modern (MonadTxpLD)
#endif
import           Pos.DHT.Model             (MonadDHT, MonadMessageDHT)
import           Pos.Slotting              (MonadSlots)
import           Pos.Ssc.Class             (MonadSscLD)
import           Pos.State                 (MonadDB)
import           Pos.Util.JsonLog          (MonadJL)
import           Pos.Util.UserSecret       (UserSecret, peekUserSecret, takeUserSecret,
                                            writeUserSecret, writeUserSecretRelease)

newtype KeyStorage m a = KeyStorage
    { getKeyStorage :: ReaderT (STM.TVar UserSecret) m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadTimed, MonadThrow, MonadSlots,
                MonadCatch, MonadIO, HasLoggerName, MonadDialog s p, WithNodeContext ssc, MonadJL,
                MonadDB ssc, CanLog, MonadMask, MonadDHT, MonadMessageDHT s, MonadSscLD ssc)

type instance ThreadId (KeyStorage m) = ThreadId m

instance Monad m => WrappedM (KeyStorage m) where
    type UnwrappedM (KeyStorage m) = ReaderT (STM.TVar UserSecret) m
    _WrappedM = iso getKeyStorage KeyStorage

instance MonadTransfer s m => MonadTransfer s (KeyStorage m)

#ifdef WITH_ROCKS
deriving instance Modern.MonadDB ssc m => Modern.MonadDB ssc (KeyStorage m)
deriving instance Modern.MonadTxpLD ssc m => Modern.MonadTxpLD ssc (KeyStorage m)
#endif
