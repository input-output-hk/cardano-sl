{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Definitions for class of monads that capture logic of processing
-- delegate certificates (proxy secret keys).

module Pos.Delegation.Holder
       ( DelegationT (..)
       , runDelegationT
       , runDelegationTFromTVar
       ) where

import           Control.Concurrent.STM.TVar (TVar, newTVarIO)
import           Control.Lens                (iso)
import           Control.Monad.Fix           (MonadFix)
import           Control.Monad.Trans.Class   (MonadTrans)
import           Mockable                    (ChannelT, Counter, Distribution, Gauge,
                                              MFunctor', Mockable (liftMockable), Promise,
                                              SharedAtomicT, SharedExclusiveT, ThreadId,
                                              liftMockableWrappedM)
import           Serokell.Util.Lens          (WrappedM (..))
import           System.Wlog                 (CanLog, HasLoggerName)
import           Universum

import           Pos.Context                 (WithNodeContext)
import           Pos.DB.Class                (MonadDB)
import           Pos.Delegation.Class        (DelegationWrap (..), MonadDelegation (..))
import           Pos.Slotting                (MonadSlots (..))
import           Pos.Ssc.Extra               (MonadSscMem (..))
import           Pos.Txp.Class               (MonadTxpLD (..))
import           Pos.Types.Utxo.Class        (MonadUtxo, MonadUtxoRead)
import           Pos.Util.JsonLog            (MonadJL (..))


type ReaderTCtx = TVar DelegationWrap

-- | Wrapper of @ReaderT (TVar DelegationWrap)@, nothing smart.
newtype DelegationT m a = DelegationT
    { getDelegationT :: ReaderT ReaderTCtx m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadFix,
                MonadThrow, MonadSlots, MonadCatch, MonadIO, MonadFail,
                HasLoggerName, WithNodeContext ssc, MonadJL,
                CanLog, MonadMask, MonadSscMem kek,
                MonadUtxoRead, MonadUtxo, MonadTxpLD ssc)

deriving instance MonadDB ssc m => MonadDB ssc (DelegationT m)

instance (Monad m) => MonadDelegation (DelegationT m) where
    askDelegationState = DelegationT ask

instance Monad m => WrappedM (DelegationT m) where
    type UnwrappedM (DelegationT m) = ReaderT ReaderTCtx m
    _WrappedM = iso getDelegationT DelegationT

type instance ThreadId (DelegationT m) = ThreadId m
type instance Promise (DelegationT m) = Promise m
type instance SharedAtomicT (DelegationT m) = SharedAtomicT m
type instance Counter (DelegationT m) = Counter m
type instance Distribution (DelegationT m) = Distribution m
type instance SharedExclusiveT (DelegationT m) = SharedExclusiveT m
type instance Gauge (DelegationT m) = Gauge m
type instance ChannelT (DelegationT m) = ChannelT m

instance ( Mockable d m
         , MFunctor' d (ReaderT ReaderTCtx m) m
         , MFunctor' d (DelegationT m) (ReaderT ReaderTCtx m)
         ) => Mockable d (DelegationT m) where
    liftMockable = liftMockableWrappedM

-- | Executes delegationT transformer creating tvar from given wrap.
runDelegationT :: MonadIO m => DelegationWrap -> DelegationT m a -> m a
runDelegationT wrap action =
    liftIO (newTVarIO wrap) >>= runReaderT (getDelegationT action)

-- | Executes delegation wrap using existing delegation wrap tvar.
runDelegationTFromTVar :: TVar DelegationWrap -> DelegationT m a -> m a
runDelegationTFromTVar var action = runReaderT (getDelegationT action) var
