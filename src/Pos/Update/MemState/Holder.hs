{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Monad transformer which implements MonadUSMem based on ReaderT.
-- It's also instance of MonadPoll.

module Pos.Update.MemState.Holder
       ( USHolder (..)
       , runUSHolder
       , runUSHolderFromTVar
       ) where

import           Control.Concurrent.STM       (TVar, newTVarIO, readTVar, writeTVar)
import           Control.Lens                 (iso)
import           Control.Monad.Base           (MonadBase (..))
import           Control.Monad.Fix            (MonadFix)
import           Control.Monad.State          (MonadState (..))
import           Control.Monad.Trans.Class    (MonadTrans)
import           Control.Monad.Trans.Control  (ComposeSt, MonadBaseControl (..),
                                               MonadTransControl (..), StM,
                                               defaultLiftBaseWith, defaultLiftWith,
                                               defaultRestoreM, defaultRestoreT)
import           Data.Default                 (Default (def))
import           Mockable                     (ChannelT, MFunctor',
                                               Mockable (liftMockable), Promise,
                                               SharedAtomicT, ThreadId,
                                               liftMockableWrappedM)
import           Serokell.Util.Lens           (WrappedM (..))
import           System.Wlog                  (CanLog, HasLoggerName)
import           Universum

import           Pos.Context                  (WithNodeContext)
import           Pos.DB.Class                 (MonadDB)
import           Pos.Delegation.Class         (MonadDelegation)
import           Pos.Slotting                 (MonadSlots (..))
import           Pos.Ssc.Extra                (MonadSscGS (..), MonadSscLD (..),
                                               MonadSscRichmen)
import           Pos.Txp.Class                (MonadTxpLD (..))
import           Pos.Types.Utxo.Class         (MonadUtxo, MonadUtxoRead)
import           Pos.Update.MemState.Class    (MonadUSMem (..))
import           Pos.Update.MemState.MemState (MemState)
import           Pos.Util.JsonLog             (MonadJL (..))

-- | Trivial monad transformer based on @ReaderT (TVar MemState)@.
newtype USHolder m a = USHolder
    { getUSHolder :: ReaderT (TVar MemState) m a
    } deriving (Functor, Applicative, Monad, MonadTrans,
                MonadThrow, MonadSlots, MonadCatch, MonadIO, MonadFail,
                HasLoggerName, WithNodeContext ssc, MonadJL,
                CanLog, MonadMask, MonadSscLD kek, MonadSscGS ssc,
                MonadUtxoRead, MonadUtxo, MonadTxpLD ssc, MonadBase io,
                MonadDelegation, MonadSscRichmen, MonadFix)

deriving instance MonadDB ssc m => MonadDB ssc (USHolder m)
type instance ThreadId (USHolder m) = ThreadId m
type instance Promise (USHolder m) = Promise m
type instance SharedAtomicT (USHolder m) = SharedAtomicT m
type instance ChannelT (USHolder m) = ChannelT m

instance ( Mockable d m
         , MFunctor' d (USHolder m) (ReaderT (TVar MemState) m)
         , MFunctor' d (ReaderT (TVar MemState) m) m
         ) => Mockable d (USHolder m) where
    liftMockable = liftMockableWrappedM

instance MonadIO m => MonadState MemState (USHolder m) where
    get = USHolder ask >>= atomically . readTVar
    put s = USHolder ask >>= atomically . flip writeTVar s

instance MonadDB ssc m => MonadUSMem (USHolder m) where
    askUSMemState = USHolder ask

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
