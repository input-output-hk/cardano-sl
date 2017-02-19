{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Pos.Txp.Holder
       (
         TxpLDHolder (..)
       , runTxpLDHolder
       , runLocalTxpLDHolder

       , TxpLDWrap (..)
       , runTxpLDHolderReader
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

import           Pos.Context               (WithNodeContext)
import           Pos.DB.Class              (MonadDB)
import           Pos.DB.Limits             (MonadDBLimits)
import           Pos.DB.Holder             (DBHolder (..))
import           Pos.Slotting.Class        (MonadSlots)
import           Pos.Ssc.Extra             (MonadSscMem)
import           Pos.Txp.Class             (MonadTxpLD (..), TxpLDWrap (..))
import           Pos.Txp.Types             (UtxoView)
import qualified Pos.Txp.Types.UtxoView    as UV
import           Pos.Types                 (HeaderHash, MonadUtxo (..),
                                            MonadUtxoRead (..), genesisHash)
import           Pos.Util.JsonLog          (MonadJL (..))

----------------------------------------------------------------------------
-- Holder
----------------------------------------------------------------------------

newtype TxpLDHolder ssc m a = TxpLDHolder
    { getTxpLDHolder :: ReaderT (TxpLDWrap ssc) m a
    } deriving (Functor, Applicative, Monad, MonadTrans,
                MonadThrow, MonadSlots, MonadCatch, MonadIO, MonadFail,
                HasLoggerName, WithNodeContext ssc, MonadJL, MonadDBLimits,
                CanLog, MonadMask, MonadSscMem ssc, MonadFix)

type instance ThreadId (TxpLDHolder ssc m) = ThreadId m
type instance Promise (TxpLDHolder ssc m) = Promise m
type instance SharedAtomicT (TxpLDHolder ssc m) = SharedAtomicT m
type instance Counter (TxpLDHolder ssc m) = Counter m
type instance Distribution (TxpLDHolder ssc m) = Distribution m
type instance SharedExclusiveT (TxpLDHolder ssc m) = SharedExclusiveT m
type instance Gauge (TxpLDHolder ssc m) = Gauge m
type instance ChannelT (TxpLDHolder ssc m) = ChannelT m

instance ( Mockable d m
         , MFunctor' d (ReaderT (TxpLDWrap ssc) m) m
         , MFunctor' d (TxpLDHolder ssc m) (ReaderT (TxpLDWrap ssc) m)
         ) => Mockable d (TxpLDHolder ssc m) where
    liftMockable = liftMockableWrappedM

deriving instance MonadDB ssc m => MonadDB ssc (TxpLDHolder ssc m)

deriving instance MonadTxpLD ssc m => MonadTxpLD ssc (DBHolder ssc m)

----------------------------------------------------------------------------
-- Useful instances
----------------------------------------------------------------------------

instance MonadIO m => MonadTxpLD ssc (TxpLDHolder ssc m) where
    getTxpLDWrap = TxpLDHolder ask
    setUtxoView uv = TxpLDHolder (asks utxoView) >>= atomically . flip STM.writeTVar uv
    setMemPool mp = TxpLDHolder (asks memPool) >>= atomically . flip STM.writeTVar mp
    getTxpLD = TxpLDHolder ask >>= \txld -> atomically $
        (,,,) <$> STM.readTVar (utxoView txld)
              <*> STM.readTVar (memPool txld)
              <*> STM.readTVar (undos txld)
              <*> STM.readTVar (ldTip txld)
    modifyTxpLD f =
        TxpLDHolder ask >>= \txld -> atomically $ do
            curUV  <- STM.readTVar (utxoView txld)
            curMP  <- STM.readTVar (memPool txld)
            curUndos <- STM.readTVar (undos txld)
            curTip <- STM.readTVar (ldTip txld)
            let (res, (newUV, newMP, newUndos, newTip))
                  = f (curUV, curMP, curUndos, curTip)
            STM.writeTVar (utxoView txld) newUV
            STM.writeTVar (memPool txld) newMP
            STM.writeTVar (undos txld) newUndos
            STM.writeTVar (ldTip txld) newTip
            return res

instance Monad m => WrappedM (TxpLDHolder ssc m) where
    type UnwrappedM (TxpLDHolder ssc m) = ReaderT (TxpLDWrap ssc) m
    _WrappedM = iso getTxpLDHolder TxpLDHolder

instance (MonadIO m, MonadThrow m) => MonadUtxoRead (TxpLDHolder ssc m) where
    utxoGet key = TxpLDHolder (asks utxoView) >>=
                   (atomically . STM.readTVar >=> UV.getTxOut key)

instance (MonadIO m, MonadUtxoRead (TxpLDHolder ssc m))
       => MonadUtxo (TxpLDHolder ssc m) where
    utxoPut key val = TxpLDHolder (asks utxoView) >>=
                       atomically . flip STM.modifyTVar' (UV.putTxOut key val)
    utxoDel key = TxpLDHolder (asks utxoView) >>=
                  atomically . flip STM.modifyTVar' (UV.delTxIn key)

runTxpLDHolder :: MonadIO m
               => UtxoView ssc -> HeaderHash -> TxpLDHolder ssc m a -> m a
runTxpLDHolder uv initTip holder =
    TxpLDWrap <$> liftIO (STM.newTVarIO uv)
              <*> liftIO (STM.newTVarIO def)
              <*> liftIO (STM.newTVarIO mempty)
              <*> liftIO (STM.newTVarIO initTip)
              >>= runReaderT (getTxpLDHolder holder)

-- | Local run needed for validation txs. For validation need only UtxoView.
runLocalTxpLDHolder :: MonadIO m
                    => TxpLDHolder ssc m a -> UtxoView ssc -> m a
runLocalTxpLDHolder holder uv =
    TxpLDWrap <$> liftIO (STM.newTVarIO uv)
              <*> liftIO (STM.newTVarIO def)
              <*> liftIO (STM.newTVarIO mempty)
              <*> liftIO (STM.newTVarIO genesisHash)
              >>= runReaderT (getTxpLDHolder holder)

runTxpLDHolderReader
    :: TxpLDWrap ssc -> TxpLDHolder ssc m a -> m a
runTxpLDHolderReader wrap holder = runReaderT (getTxpLDHolder holder) wrap
