{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Pos.Wallet.Web.State.Holder
       ( WalletWebDB
       , runWalletWebDB
       ) where

import           Universum

import           Control.Lens               (iso)
import           Control.Monad.Trans        (MonadTrans (..))
import           Control.TimeWarp.Rpc       (MonadDialog, MonadTransfer)
import           Control.TimeWarp.Timed     (MonadTimed, ThreadId)
import           Serokell.Util.Lens         (WrappedM (..))
import           System.Wlog                (CanLog, HasLoggerName)

import           Pos.Context                (WithNodeContext)
#ifdef WITH_ROCKS
import qualified Pos.Modern.DB              as Modern (MonadDB)
import qualified Pos.Modern.Txp.Class       as Modern (MonadTxpLD)
#endif
import           Pos.DHT.Model              (MonadDHT, MonadMessageDHT,
                                             WithDefaultMsgHeader)
import           Pos.Slotting               (MonadSlots)
import           Pos.Ssc.Class              (MonadSscLD)
import           Pos.State                  (MonadDB)
import           Pos.Statistics             (MonadStats)
import           Pos.Txp.LocalData          (MonadTxLD)
import           Pos.Util.JsonLog           (MonadJL)
import           Pos.Wallet.KeyStorage      (MonadKeys)
import           Pos.Wallet.Web.State.State (MonadWalletWebDB (..), WalletState)
import           Pos.WorkMode               ()

-- | Holder for web wallet data
newtype WalletWebDB m a = WalletWebDB
    { getWalletWebDB :: ReaderT WalletState m a
    } deriving (Functor, Applicative, Monad, MonadTimed, MonadThrow, MonadCatch,
                MonadMask, MonadIO, MonadDB ssc, HasLoggerName, WithNodeContext ssc,
                MonadDialog s p, MonadDHT, MonadMessageDHT s, MonadSlots, MonadSscLD ssc,
#ifdef WITH_ROCKS
                Modern.MonadDB ssc, Modern.MonadTxpLD ssc,
#endif
                WithDefaultMsgHeader, MonadJL, CanLog, MonadTxLD, MonadStats, MonadKeys)

instance Monad m => WrappedM (WalletWebDB m) where
    type UnwrappedM (WalletWebDB m) = ReaderT WalletState m
    _WrappedM = iso getWalletWebDB WalletWebDB

instance MonadTrans WalletWebDB where
    lift = WalletWebDB . lift

instance MonadTransfer s m => MonadTransfer s (WalletWebDB m)

type instance ThreadId (WalletWebDB m) = ThreadId m

-- | Instance for generic web wallet class
instance Monad m => MonadWalletWebDB (WalletWebDB m) where
    getWalletWebState = WalletWebDB ask

-- | Execute `WalletWebDB` action with given `WalletState`
runWalletWebDB :: WalletState -> WalletWebDB m a -> m a
runWalletWebDB ws = flip runReaderT ws . getWalletWebDB
