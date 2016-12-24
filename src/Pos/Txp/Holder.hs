{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Pos.Txp.Holder
       (
         TxpLDHolder (..)
       , runTxpLDHolder
       , runLocalTxpLDHolder
       ) where

import qualified Control.Concurrent.STM      as STM
import           Control.Lens                (iso)
import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Catch         (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Reader        (ReaderT (ReaderT))
import           Control.Monad.Trans.Class   (MonadTrans)
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..), StM,
                                              defaultLiftBaseWith, defaultLiftWith,
                                              defaultRestoreM, defaultRestoreT)
import           Control.TimeWarp.Rpc        (MonadDialog, MonadTransfer (..))
import           Control.TimeWarp.Timed      (MonadTimed (..), ThreadId)
import           Data.Default                (def)
import           Serokell.Util.Lens          (WrappedM (..))
import           System.Wlog                 (CanLog, HasLoggerName)
import           Universum

import           Pos.Context                 (WithNodeContext)
import qualified Pos.DB.Class                (MonadDB)
import           Pos.Slotting                (MonadSlots (..))
import           Pos.Ssc.Class.LocalData     (MonadSscLD (..))
import           Pos.Ssc.Extra               (MonadSscGS (..), MonadSscLDM (..))
import           Pos.State                   (MonadDB (..))
import           Pos.Txp.Class               (MonadTxpLD (..))
import           Pos.Txp.LocalData           (MonadTxLD (..))
import           Pos.Txp.Types               (MemPool, UtxoView)
import qualified Pos.Txp.Types.UtxoView      as UV
import           Pos.Types                   (HeaderHash, MonadUtxo (..),
                                              MonadUtxoRead (..), TxId, TxOut,
                                              genesisHash)
import           Pos.Util.JsonLog            (MonadJL (..))

----------------------------------------------------------------------------
-- Holder
----------------------------------------------------------------------------
data TxpLDWrap ssc = TxpLDWrap
    {
      utxoView :: !(STM.TVar (UtxoView ssc))
    , memPool  :: !(STM.TVar MemPool)
    , undos    :: !(STM.TVar (HashMap TxId [TxOut]))
    , ldTip    :: !(STM.TVar (HeaderHash ssc))
    }

newtype TxpLDHolder ssc m a = TxpLDHolder
    { getTxpLDHolder :: ReaderT (TxpLDWrap ssc) m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadTimed, MonadThrow, MonadSlots,
                MonadCatch, MonadIO, HasLoggerName, MonadDialog s p, WithNodeContext ssc, MonadJL,
                MonadDB ssc, CanLog, MonadMask, MonadTxLD, MonadSscLD ssc, MonadSscGS ssc,
                MonadSscLDM ssc)

instance MonadTransfer s m => MonadTransfer s (TxpLDHolder ssc m)
type instance ThreadId (TxpLDHolder ssc m) = ThreadId m

instance MonadBase IO m => MonadBase IO (TxpLDHolder ssc m) where
    liftBase = lift . liftBase

instance MonadTransControl (TxpLDHolder ssc) where
    type StT (TxpLDHolder ssc) a = StT (ReaderT (TxpLDWrap ssc)) a
    liftWith = defaultLiftWith TxpLDHolder getTxpLDHolder
    restoreT = defaultRestoreT TxpLDHolder

instance MonadBaseControl IO m => MonadBaseControl IO (TxpLDHolder ssc m) where
    type StM (TxpLDHolder ssc m) a = ComposeSt (TxpLDHolder ssc) m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM

deriving instance Pos.DB.Class.MonadDB ssc m => Pos.DB.Class.MonadDB ssc (TxpLDHolder ssc m)

----------------------------------------------------------------------------
-- Useful instances
----------------------------------------------------------------------------
instance MonadIO m => MonadTxpLD ssc (TxpLDHolder ssc m) where
    getUtxoView = TxpLDHolder (asks utxoView) >>= atomically . STM.readTVar
    setUtxoView uv = TxpLDHolder (asks utxoView) >>= atomically . flip STM.writeTVar uv
    getMemPool = TxpLDHolder (asks memPool) >>= atomically . STM.readTVar
    setMemPool mp = TxpLDHolder (asks memPool) >>= atomically . flip STM.writeTVar mp
    getTxpLD = TxpLDHolder ask >>= \txld -> atomically $
        (,,,) <$> STM.readTVar (utxoView txld)
              <*> STM.readTVar (memPool txld)
              <*> STM.readTVar (undos txld)
              <*> STM.readTVar (ldTip txld)
    modifyTxpLD f = TxpLDHolder ask >>= \txld -> atomically $ do
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
               => UtxoView ssc -> HeaderHash ssc -> TxpLDHolder ssc m a -> m a
runTxpLDHolder uv initTip holder = TxpLDWrap
                       <$> liftIO (STM.newTVarIO uv)
                       <*> liftIO (STM.newTVarIO def)
                       <*> liftIO (STM.newTVarIO mempty)
                       <*> liftIO (STM.newTVarIO initTip)
                       >>= runReaderT (getTxpLDHolder holder)

-- | Local run needed for validation txs. For validation need only UtxoView.
runLocalTxpLDHolder :: MonadIO m
                    => TxpLDHolder ssc m a -> UtxoView ssc -> m a
runLocalTxpLDHolder holder uv = TxpLDWrap
                       <$> liftIO (STM.newTVarIO uv)
                       <*> liftIO (STM.newTVarIO def)
                       <*> liftIO (STM.newTVarIO mempty)
                       <*> liftIO (STM.newTVarIO genesisHash)
                       >>= runReaderT (getTxpLDHolder holder)
