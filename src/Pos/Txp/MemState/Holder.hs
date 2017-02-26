{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Monad transformer which implements MonadTxpMem based on ReaderT.

module Pos.Txp.MemState.Holder
       ( TxpHolder (..)
       , runTxpHolder
       , runLocalTxpHolder

       , runTxpHolderReader
       ) where

import qualified Control.Concurrent.STM    as STM
import           Control.Lens              (iso)
import           Control.Monad.Catch       (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Fix         (MonadFix)
import           Control.Monad.Reader      (ReaderT (ReaderT))
import           Control.Monad.Trans.Class (MonadTrans)
import           Data.Default              (def)
import           Mockable                  (ChannelT, Counter, Distribution, Gauge, Gauge,
                                            MFunctor', Mockable (liftMockable), Promise,
                                            SharedAtomicT, SharedExclusiveT,
                                            SharedExclusiveT, ThreadId,
                                            liftMockableWrappedM)
import           Serokell.Util.Lens        (WrappedM (..))
import           System.Wlog               (CanLog, HasLoggerName)
import           Universum

import           Pos.Context.Class         (WithNodeContext)
import           Pos.DB.Class              (MonadDB)
import           Pos.DB.Holder             (DBHolder (..))
import           Pos.Slotting.Class        (MonadSlots, MonadSlotsData)
import           Pos.Ssc.Extra             (MonadSscMem)
import           Pos.Types                 (HeaderHash, genesisHash)
import           Pos.Util.JsonLog          (MonadJL (..))

import           Pos.Txp.MemState.Class    (MonadTxpMem (..))
import           Pos.Txp.MemState.Types    (TxpLocalData (..))
import           Pos.Txp.Toil.Types         (UtxoView)

----------------------------------------------------------------------------
-- Holder
----------------------------------------------------------------------------

newtype TxpHolder m a = TxpHolder
    { getTxpHolder :: ReaderT TxpLocalData m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadThrow,
                MonadSlotsData, MonadSlots, MonadCatch, MonadIO, MonadFail,
                HasLoggerName, WithNodeContext ssc, MonadJL,
                CanLog, MonadMask, MonadSscMem ssc, MonadFix)

type instance ThreadId (TxpHolder m) = ThreadId m
type instance Promise (TxpHolder m) = Promise m
type instance SharedAtomicT (TxpHolder m) = SharedAtomicT m
type instance Counter (TxpHolder m) = Counter m
type instance Distribution (TxpHolder m) = Distribution m
type instance SharedExclusiveT (TxpHolder m) = SharedExclusiveT m
type instance Gauge (TxpHolder m) = Gauge m
type instance ChannelT (TxpHolder m) = ChannelT m

instance ( Mockable d m
         , MFunctor' d (ReaderT TxpLocalData m) m
         , MFunctor' d (TxpHolder m) (ReaderT TxpLocalData m)
         ) => Mockable d (TxpHolder m) where
    liftMockable = liftMockableWrappedM

deriving instance MonadDB ssc m => MonadDB ssc (TxpHolder m)

deriving instance MonadTxpMem m => MonadTxpMem (DBHolder ssc m)

----------------------------------------------------------------------------
-- Useful instances
----------------------------------------------------------------------------

instance Monad m => MonadTxpMem (TxpHolder m) where
    askTxpMem = TxpHolder ask

instance Monad m => WrappedM (TxpHolder m) where
    type UnwrappedM (TxpHolder m) = ReaderT TxpLocalData m
    _WrappedM = iso getTxpHolder TxpHolder

runTxpHolder
    :: MonadIO m
    => UtxoView -> HeaderHash -> TxpHolder m a -> m a
runTxpHolder uv initTip holder = TxpLocalData
    <$> liftIO (STM.newTVarIO uv)
    <*> liftIO (STM.newTVarIO def)
    <*> liftIO (STM.newTVarIO mempty)
    <*> liftIO (STM.newTVarIO initTip)
    >>= runReaderT (getTxpHolder holder)

-- | Local run needed for validation txs. For validation need only UtxoView.
runLocalTxpHolder
    :: MonadIO m
    => TxpHolder m a -> UtxoView -> m a
runLocalTxpHolder holder uv = TxpLocalData
    <$> liftIO (STM.newTVarIO uv)
    <*> liftIO (STM.newTVarIO def)
    <*> liftIO (STM.newTVarIO mempty)
    <*> liftIO (STM.newTVarIO genesisHash)
    >>= runReaderT (getTxpHolder holder)

runTxpHolderReader
    :: TxpLocalData -> TxpHolder m a -> m a
runTxpHolderReader wrap holder = runReaderT (getTxpHolder holder) wrap
