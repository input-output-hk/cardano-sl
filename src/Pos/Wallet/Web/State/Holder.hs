{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}

module Pos.Wallet.Web.State.Holder
       ( WalletWebDB
       , runWalletWebDB
       ) where

import           Universum

import           Control.Lens               (iso)
import           Control.Monad.Trans        (MonadTrans (..))
import           Control.TimeWarp.Rpc       (MonadDialog, MonadTransfer)
import           Control.TimeWarp.Timed     (MonadTimed, ThreadId)
import           Pos.DHT                    (MonadDHT, MonadMessageDHT,
                                             WithDefaultMsgHeader)
import           Serokell.Util.Lens         (WrappedM (..))
import           System.Wlog                (CanLog, HasLoggerName)

import           Pos.Context                (WithNodeContext)
#ifdef WITH_ROCKS
import qualified Pos.Modern.DB              as Modern (MonadDB)
#endif
import           Pos.Slotting               (MonadSlots)
import           Pos.Ssc.Class              (MonadSscLD)
import           Pos.State                  (MonadDB)
import           Pos.Statistics             (MonadStats)
import           Pos.Txp.LocalData          (MonadTxLD)
import           Pos.Util.JsonLog           (MonadJL)
import           Pos.Wallet.Web.State.State (MonadWalletWebDB (..), WalletState)
import           Pos.WorkMode               ()

-- | Holder for web wallet data
newtype WalletWebDB m a = WalletWebDB
    { getWalletWebDB :: ReaderT WalletState m a
    } deriving (Functor, Applicative, Monad, MonadTimed, MonadThrow, MonadCatch,
                MonadMask, MonadIO, MonadDB ssc, HasLoggerName, WithNodeContext ssc,
                MonadDialog p, MonadDHT, MonadMessageDHT, MonadSlots, MonadSscLD ssc,
#ifdef WITH_ROCKS
                Modern.MonadDB ssc,
#endif
                WithDefaultMsgHeader, MonadJL, CanLog, MonadTxLD, MonadStats)

instance Monad m => WrappedM (WalletWebDB m) where
    type UnwrappedM (WalletWebDB m) = ReaderT WalletState m
    _WrappedM = iso getWalletWebDB WalletWebDB

instance MonadTrans WalletWebDB where
    lift = WalletWebDB . lift

instance MonadTransfer m => MonadTransfer (WalletWebDB m)

type instance ThreadId (WalletWebDB m) = ThreadId m

-- | Instance for generic web wallet class
instance Monad m => MonadWalletWebDB (WalletWebDB m) where
    getWalletWebState = WalletWebDB ask

-- | Execute `WalletWebDB` action with given `WalletState`
runWalletWebDB :: WalletState -> WalletWebDB m a -> m a
runWalletWebDB ws = flip runReaderT ws . getWalletWebDB
