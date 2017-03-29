{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Monad transformer which implements MonadTxpMem based on ReaderT.

module Pos.Txp.MemState.Holder
       ( TxpHolder (..)
       , mkTxpLocalData
       , runTxpHolder
       ) where

import qualified Control.Concurrent.STM    as STM
import           Control.Lens              (iso)
import           Control.Monad.Catch       (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Fix         (MonadFix)
import           Control.Monad.Reader      (ReaderT (ReaderT))
import           Control.Monad.Trans.Class (MonadTrans)
import           Data.Default              (Default (def))
import           Mockable                  (ChannelT, Counter, Distribution, Gauge, Gauge,
                                            MFunctor', Mockable (liftMockable), Promise,
                                            SharedAtomicT, SharedExclusiveT,
                                            SharedExclusiveT, ThreadId,
                                            liftMockableWrappedM)
import           Serokell.Util.Lens        (WrappedM (..))
import           System.Wlog               (CanLog, HasLoggerName)
import           Universum

import           Pos.Communication.Relay   (MonadRelayMem)
import           Pos.Context.Class         (WithNodeContext)
import           Pos.DB.Class              (MonadDB)
import           Pos.DB.Limits             (MonadDBLimits)
import           Pos.DHT.MemState          (MonadDhtMem)
import           Pos.Reporting             (MonadReportingMem)
import           Pos.Shutdown              (MonadShutdownMem)
import           Pos.Slotting.Class        (MonadSlots)
import           Pos.Slotting.MemState     (MonadSlotsData)
import           Pos.Ssc.Extra             (MonadSscMem)
import           Pos.Types                 (HeaderHash)
import           Pos.Util.JsonLog          (MonadJL (..))

import           Pos.Txp.MemState.Class    (MonadTxpMem (..))
import           Pos.Txp.MemState.Types    (GenericTxpLocalData (..))
import           Pos.Txp.Toil.Types        (UtxoModifier)
import           Pos.Util.Context          (MonadContext (..))

----------------------------------------------------------------------------
-- Holder
----------------------------------------------------------------------------

newtype TxpHolder ext m a = TxpHolder
    { getTxpHolder :: ReaderT (GenericTxpLocalData ext) m a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadTrans
               , MonadThrow
               , MonadSlotsData
               , MonadSlots
               , MonadCatch
               , MonadIO
               , MonadFail
               , HasLoggerName
               , WithNodeContext ssc
               , MonadJL
               , CanLog
               , MonadMask
               , MonadSscMem ssc
               , MonadFix
               , MonadDhtMem
               , MonadReportingMem
               , MonadRelayMem
               , MonadShutdownMem
               , MonadDB
               , MonadDBLimits
               )

instance MonadContext m => MonadContext (TxpHolder x m) where
    type ContextType (TxpHolder x m) = ContextType m

type instance ThreadId (TxpHolder x m) = ThreadId m
type instance Promise (TxpHolder x m) = Promise m
type instance SharedAtomicT (TxpHolder x m) = SharedAtomicT m
type instance Counter (TxpHolder x m) = Counter m
type instance Distribution (TxpHolder x m) = Distribution m
type instance SharedExclusiveT (TxpHolder x m) = SharedExclusiveT m
type instance Gauge (TxpHolder x m) = Gauge m
type instance ChannelT (TxpHolder x m) = ChannelT m

instance ( Mockable d m
         , MFunctor' d (ReaderT (GenericTxpLocalData e) m) m
         , MFunctor' d (TxpHolder e m) (ReaderT (GenericTxpLocalData e) m)
         ) => Mockable d (TxpHolder e m) where
    liftMockable = liftMockableWrappedM

----------------------------------------------------------------------------
-- Useful instances
----------------------------------------------------------------------------

instance Monad m => MonadTxpMem ext (TxpHolder ext m) where
    askTxpMem = TxpHolder ask

instance Monad m => WrappedM (TxpHolder ext m) where
    type UnwrappedM (TxpHolder ext m) = ReaderT (GenericTxpLocalData ext) m
    _WrappedM = iso getTxpHolder TxpHolder

mkTxpLocalData
    :: (Default e, MonadIO m)
    => UtxoModifier -> HeaderHash -> m (GenericTxpLocalData e)
mkTxpLocalData uv initTip = TxpLocalData
    <$> liftIO (STM.newTVarIO uv)
    <*> liftIO (STM.newTVarIO def)
    <*> liftIO (STM.newTVarIO mempty)
    <*> liftIO (STM.newTVarIO initTip)
    <*> liftIO (STM.newTVarIO def)

runTxpHolder :: GenericTxpLocalData ext -> TxpHolder ext m a -> m a
runTxpHolder ld holder = runReaderT (getTxpHolder holder) ld
