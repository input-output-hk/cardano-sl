{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Monad transformer which implements MonadUSMem based on ReaderT.
-- It's also instance of MonadPoll.

module Pos.Update.MemState.Holder
       ( USHolder (..)
       , runUSHolder
       , runUSHolderFromVar
       ) where

import           Control.Lens                (iso)
import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Fix           (MonadFix)
import           Control.Monad.Trans.Class   (MonadTrans)
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..), StM,
                                              defaultLiftBaseWith, defaultLiftWith,
                                              defaultRestoreM, defaultRestoreT)
import           Mockable                    (ChannelT, Counter, Distribution, Gauge,
                                              MFunctor', Mockable (liftMockable), Promise,
                                              SharedAtomicT, SharedExclusiveT, ThreadId,
                                              liftMockableWrappedM)
import           Serokell.Util.Lens          (WrappedM (..))
import           System.Wlog                 (CanLog, HasLoggerName)
import           Universum

import           Pos.Context                 (WithNodeContext)
import           Pos.DB.Class                (MonadDB)
import           Pos.DB.Limits               (MonadDBLimits)
import           Pos.Delegation.Class        (MonadDelegation)
import           Pos.Slotting.Class          (MonadSlots, MonadSlotsData)
import           Pos.Ssc.Extra               (MonadSscMem)
import           Pos.Txp.Class               (MonadTxpLD (..))
import           Pos.Types.Utxo.Class        (MonadUtxo, MonadUtxoRead)
import           Pos.Update.MemState.Class   (MonadUSMem (..))
import           Pos.Update.MemState.Types   (MemVar, newMemVar)
import           Pos.Util.JsonLog            (MonadJL (..))

----------------------------------------------------------------------------
-- Transformer
----------------------------------------------------------------------------

-- | Trivial monad transformer based on @ReaderT (MemVar)@.
newtype USHolder m a = USHolder
    { getUSHolder :: ReaderT MemVar m a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadTrans
               , MonadFail
               , MonadThrow
               , MonadSlotsData
               , MonadSlots
               , MonadCatch
               , MonadIO
               , HasLoggerName
               , WithNodeContext ssc
               , MonadJL
               , CanLog
               , MonadMask
               , MonadSscMem ssc
               , MonadUtxoRead
               , MonadUtxo
               , MonadTxpLD ssc
               , MonadBase io
               , MonadDelegation
               , MonadFix
               , MonadDBLimits
               )

----------------------------------------------------------------------------
-- Common instances used all over the code
----------------------------------------------------------------------------

deriving instance MonadDB ssc m => MonadDB ssc (USHolder m)
type instance ThreadId (USHolder m) = ThreadId m
type instance Promise (USHolder m) = Promise m
type instance SharedAtomicT (USHolder m) = SharedAtomicT m
type instance Counter (USHolder m) = Counter m
type instance Distribution (USHolder m) = Distribution m
type instance SharedExclusiveT (USHolder m) = SharedExclusiveT m
type instance Gauge (USHolder m) = Gauge m
type instance ChannelT (USHolder m) = ChannelT m

instance ( Mockable d m
         , MFunctor' d (USHolder m) (ReaderT (MemVar) m)
         , MFunctor' d (ReaderT (MemVar) m) m
         ) => Mockable d (USHolder m) where
    liftMockable = liftMockableWrappedM

instance Monad m => WrappedM (USHolder m) where
    type UnwrappedM (USHolder m) = ReaderT (MemVar) m
    _WrappedM = iso getUSHolder USHolder

instance MonadTransControl USHolder where
    type StT (USHolder) a = StT (ReaderT (MemVar)) a
    liftWith = defaultLiftWith USHolder getUSHolder
    restoreT = defaultRestoreT USHolder

instance MonadBaseControl IO m => MonadBaseControl IO (USHolder m) where
    type StM (USHolder m) a = ComposeSt USHolder m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM

----------------------------------------------------------------------------
-- MonadUSMem
----------------------------------------------------------------------------

instance Monad m => MonadUSMem (USHolder m) where
    askUSMemVar = USHolder ask

----------------------------------------------------------------------------
-- Runners
----------------------------------------------------------------------------

-- | Run USHolder using default (empty) MemState.
runUSHolder :: MonadIO m => USHolder m a -> m a
runUSHolder action = liftIO newMemVar >>= runReaderT (getUSHolder action)

-- | Run USHolder using existing MemVar.
runUSHolderFromVar :: MemVar -> USHolder m a -> m a
runUSHolderFromVar var action = runReaderT (getUSHolder action) var
