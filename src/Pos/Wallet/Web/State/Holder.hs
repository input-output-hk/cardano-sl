{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

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
import qualified Pos.DB                     as Modern
import           Pos.Delegation.Class       (MonadDelegation)
import           Pos.DHT.Model              (MonadDHT, MonadMessageDHT,
                                             WithDefaultMsgHeader)
import           Pos.Slotting               (MonadSlots)
import           Pos.Txp.Class              (MonadTxpLD)
import           Pos.Update                 (MonadUS)

import           Pos.Wallet.Context         (WithWalletContext)
import           Pos.Wallet.KeyStorage      (MonadKeys)
import           Pos.Wallet.State           (MonadWalletDB)
import           Pos.Wallet.WalletMode      (MonadBalances, MonadTxHistory)

import           Pos.Wallet.Web.State.State (MonadWalletWebDB (..), WalletState)

-- | Holder for web wallet data
newtype WalletWebDB m a = WalletWebDB
    { getWalletWebDB :: ReaderT WalletState m a
    } deriving (Functor, Applicative, Monad, MonadTimed, MonadThrow,
                MonadCatch, MonadMask, MonadIO, MonadFail, HasLoggerName,
                MonadWalletDB, WithWalletContext, MonadDialog s p,
                MonadDHT, MonadMessageDHT s, MonadSlots,
                WithDefaultMsgHeader, CanLog, MonadKeys, MonadBalances,
                MonadTxHistory, WithNodeContext ssc, MonadUS,
                Modern.MonadDB ssc, MonadTxpLD ssc, MonadDelegation)

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
