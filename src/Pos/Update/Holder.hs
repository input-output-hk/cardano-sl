{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | MonadTransformer which implements MonadUS based on ReaderT.

module Pos.Update.Holder
       ( USHolder (..)
       , runUSHolder
       , runUSHolderFromTVar
       ) where

import           Control.Concurrent.STM      (TVar, newTVarIO)
import           Control.Lens                (iso)
import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Trans.Class   (MonadTrans)
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..), StM,
                                              defaultLiftBaseWith, defaultLiftWith,
                                              defaultRestoreM, defaultRestoreT)
import           Control.TimeWarp.Rpc        (MonadDialog, MonadTransfer (..))
import           Control.TimeWarp.Timed      (MonadTimed (..), ThreadId)
import           Data.Default                (Default (def))
import           Serokell.Util.Lens          (WrappedM (..))
import           System.Wlog                 (CanLog, HasLoggerName)
import           Universum

import           Pos.Context                 (WithNodeContext)
import           Pos.DB.Class                (MonadDB)
import           Pos.Delegation.Class        (MonadDelegation)
import           Pos.Slotting                (MonadSlots (..))
import           Pos.Ssc.Extra               (MonadSscGS (..), MonadSscLD (..))
import           Pos.Txp.Class               (MonadTxpLD (..))
import           Pos.Types.Utxo.Class        (MonadUtxo, MonadUtxoRead)
import           Pos.Update.Class            (MonadUS (..))
import           Pos.Update.MemState         (MemState)
import           Pos.Util.JsonLog            (MonadJL (..))

-- | Trivial monad transformer based on @ReaderT (TVar MemState)@.
newtype USHolder m a = USHolder
    { getUSHolder :: ReaderT (TVar MemState) m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadTimed,
                MonadThrow, MonadSlots, MonadCatch, MonadIO, MonadFail,
                HasLoggerName, MonadDialog s p, WithNodeContext ssc, MonadJL,
                MonadDB ssc, CanLog, MonadMask, MonadSscLD kek, MonadSscGS ssc,
                MonadUtxoRead, MonadUtxo, MonadTxpLD ssc, MonadBase io,
                MonadDelegation)

instance Monad m => MonadUS (USHolder m) where
    askUSMemState = USHolder ask

instance MonadTransfer s m => MonadTransfer s (USHolder m)

type instance ThreadId (USHolder m) = ThreadId m

instance Monad m => WrappedM (USHolder m) where
    type UnwrappedM (USHolder m) = ReaderT (TVar MemState) m
    _WrappedM = iso getUSHolder USHolder

instance MonadTransControl USHolder where
    type StT (USHolder) a = StT (ReaderT (TVar MemState)) a
    liftWith = defaultLiftWith USHolder getUSHolder
    restoreT = defaultRestoreT USHolder

instance MonadBaseControl IO m => MonadBaseControl IO (USHolder m) where
    type StM (USHolder m) a = ComposeSt USHolder m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM

-- | Run USHolder using default (empty) MemState.
runUSHolder :: MonadIO m => USHolder m a -> m a
runUSHolder action =
    liftIO (newTVarIO def) >>= runReaderT (getUSHolder action)

-- | Run USHolder using existing TVar with MemState.
runUSHolderFromTVar :: TVar MemState -> USHolder m a -> m a
runUSHolderFromTVar var action = runReaderT (getUSHolder action) var
