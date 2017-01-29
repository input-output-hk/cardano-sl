{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Pos.Wallet.Web.State.Holder
       ( WalletWebDB (..)
       , runWalletWebDB
       , getWalletState
       ) where

import           Universum

import           Control.Lens               (iso)
import           Control.Monad.Trans        (MonadTrans (..))
import           Mockable                   (ChannelT, Counter, Distribution, Gauge,
                                             Gauge, MFunctor', Mockable (liftMockable),
                                             Promise, SharedAtomicT, SharedExclusiveT,
                                             SharedExclusiveT, ThreadId,
                                             liftMockableWrappedM)
import           Serokell.Util.Lens         (WrappedM (..))
import           System.Wlog                (CanLog, HasLoggerName)

import           Pos.Context                (WithNodeContext)
import           Pos.DB                     (MonadDB)
import           Pos.Delegation.Class       (MonadDelegation)
import           Pos.DHT.Model              (MonadDHT)
import           Pos.Slotting               (MonadSlots)
import           Pos.Txp.Class              (MonadTxpLD)
import           Pos.Update                 (MonadPollRead, MonadUSMem)

import           Pos.Wallet.Context         (WithWalletContext)
import           Pos.Wallet.KeyStorage      (MonadKeys)
import           Pos.Wallet.State           (MonadWalletDB)
import           Pos.Wallet.Web.State.State (MonadWalletWebDB (..), WalletState)

-- | Holder for web wallet data
newtype WalletWebDB m a = WalletWebDB
    { getWalletWebDB :: ReaderT WalletState m a
    } deriving (Functor, Applicative, Monad, MonadThrow,
                MonadCatch, MonadMask, MonadIO, MonadFail, HasLoggerName,
                MonadWalletDB, WithWalletContext, MonadDHT, MonadSlots, MonadTrans,
                CanLog, MonadKeys, WithNodeContext ssc, MonadUSMem, MonadPollRead,
                MonadTxpLD ssc, MonadDelegation)

deriving instance MonadDB ssc m => MonadDB ssc (WalletWebDB m)
instance Monad m => WrappedM (WalletWebDB m) where
    type UnwrappedM (WalletWebDB m) = ReaderT WalletState m
    _WrappedM = iso getWalletWebDB WalletWebDB

type instance ThreadId (WalletWebDB m) = ThreadId m
type instance Promise (WalletWebDB m) = Promise m
type instance SharedAtomicT (WalletWebDB m) = SharedAtomicT m
type instance Counter (WalletWebDB m) = Counter m
type instance Distribution (WalletWebDB m) = Distribution m
type instance SharedExclusiveT (WalletWebDB m) = SharedExclusiveT m
type instance Gauge (WalletWebDB m) = Gauge m
type instance ChannelT (WalletWebDB m) = ChannelT m

instance ( Mockable d m
         , MFunctor' d (WalletWebDB m) (ReaderT WalletState m)
         , MFunctor' d (ReaderT WalletState m) m
         ) => Mockable d (WalletWebDB m) where
    liftMockable = liftMockableWrappedM

-- | Instance for generic web wallet class
instance Monad m => MonadWalletWebDB (WalletWebDB m) where
    getWalletWebState = WalletWebDB ask

-- | Execute `WalletWebDB` action with given `WalletState`
runWalletWebDB :: WalletState -> WalletWebDB m a -> m a
runWalletWebDB ws = flip runReaderT ws . getWalletWebDB

getWalletState :: Monad m => WalletWebDB m WalletState
getWalletState = WalletWebDB ask
