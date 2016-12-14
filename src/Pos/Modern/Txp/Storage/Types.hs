{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Pos.Modern.Txp.Storage.Types
       (
         MonadUtxoRead (..)
       , MonadUtxo (..)
       , MonadTxpLD (..)
       , TxpLDHolder (..)
       , runTxpLDHolder
       ) where
import qualified Control.Concurrent.STM          as STM
import           Control.Lens                    (iso)
import           Control.Monad.Base              (MonadBase (..))
import           Control.Monad.Catch             (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Reader            (ReaderT (ReaderT))
import           Control.Monad.Trans.Class       (MonadTrans)
import           Control.Monad.Trans.Control     (ComposeSt, MonadBaseControl (..),
                                                  MonadTransControl (..), StM,
                                                  defaultLiftBaseWith, defaultLiftWith,
                                                  defaultRestoreM, defaultRestoreT)
import           Control.TimeWarp.Rpc            (MonadDialog, MonadTransfer (..))
import           Control.TimeWarp.Timed          (MonadTimed (..), ThreadId)
import qualified Data.HashSet                    as HS (empty)
import           Serokell.Util.Lens              (WrappedM (..))
import           System.Wlog                     (CanLog, HasLoggerName)
import           Universum

import           Pos.Context                     (WithNodeContext)
import           Pos.Slotting                    (MonadSlots (..))
import           Pos.Ssc.Class.LocalData         (MonadSscLD (..))
import           Pos.Ssc.Class.Types             (Ssc (SscLocalData))
import           Pos.State                       (MonadDB (..))
import           Pos.Txp.LocalData               (MonadTxLD (..))
import           Pos.Util.JsonLog                (MonadJL (..))

import qualified Pos.Modern.DB                   as Modern
import           Pos.Modern.Txp.Storage.UtxoView (UtxoView)
import qualified Pos.Modern.Txp.Storage.UtxoView as UV
import           Pos.Types                       (Tx, TxIn (..), TxOut)


class Monad m => MonadUtxoRead ssc m | m -> ssc where
    getTxOut :: TxIn -> m (Maybe TxOut)

class MonadUtxoRead ssc m => MonadUtxo ssc m | m -> ssc where
    putTxOut :: TxIn -> TxOut -> m ()
    delTxIn :: TxIn -> m ()

type MemPool = HashSet Tx

class Monad m => MonadTxpLD ssc m | m -> ssc where
    getUtxoView :: m (UtxoView ssc)
    getMemPool  :: m MemPool
    setUtxoView :: UtxoView ssc -> m ()
    setMemPool  :: MemPool -> m ()

----------------------------------------------------------------------------
-- Holder
----------------------------------------------------------------------------
data TxpLDWrap ssc = TxpLDWrap
    {
      utxoView :: !(STM.TVar (UtxoView ssc))
    , memPool  :: !(STM.TVar MemPool)
    }

newtype TxpLDHolder ssc m a = TxpLDHolder
    { getTxpLDHolder :: ReaderT (TxpLDWrap ssc) m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadTimed, MonadThrow, MonadSlots,
                MonadCatch, MonadIO, HasLoggerName, MonadDialog p, WithNodeContext ssc, MonadJL,
                MonadDB ssc, CanLog, MonadMask)

instance MonadTransfer m => MonadTransfer (TxpLDHolder ssc m)
type instance ThreadId (TxpLDHolder ssc m) = ThreadId m


instance MonadBase IO m => MonadBase IO (TxpLDHolder ssc m) where
    liftBase = lift . liftBase

instance MonadTransControl (TxpLDHolder ssc) where
    type StT (TxpLDHolder ssc) a = StT (ReaderT (STM.TVar (SscLocalData ssc))) a
    liftWith = defaultLiftWith TxpLDHolder getTxpLDHolder
    restoreT = defaultRestoreT TxpLDHolder

instance MonadBaseControl IO m => MonadBaseControl IO (TxpLDHolder ssc m) where
    type StM (TxpLDHolder ssc m) a = ComposeSt (TxpLDHolder ssc) m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM

instance (Monad m, MonadSscLD ssc m) =>
         MonadSscLD ssc (TxpLDHolder ssc m) where
    getLocalData = lift getLocalData
    setLocalData = lift . setLocalData

instance MonadTxLD m => MonadTxLD (TxpLDHolder ssc m) where
    getTxLocalData = lift getTxLocalData
    setTxLocalData = lift . setTxLocalData

deriving instance Modern.MonadDB ssc m => Modern.MonadDB ssc (TxpLDHolder ssc m)

----------------------------------------------------------------------------
-- Useful instances
----------------------------------------------------------------------------
instance MonadIO m => MonadTxpLD ssc (TxpLDHolder ssc m) where
    getUtxoView = TxpLDHolder (asks utxoView) >>= atomically . STM.readTVar
    setUtxoView uv = TxpLDHolder (asks utxoView) >>= atomically . flip STM.writeTVar uv
    getMemPool = TxpLDHolder (asks memPool) >>= atomically . STM.readTVar
    setMemPool mp = TxpLDHolder (asks memPool) >>= atomically . flip STM.writeTVar mp

instance Monad m => WrappedM (TxpLDHolder ssc m) where
    type UnwrappedM (TxpLDHolder ssc m) = ReaderT (TxpLDWrap ssc) m
    _WrappedM = iso getTxpLDHolder TxpLDHolder

instance MonadIO m => MonadUtxoRead ssc (TxpLDHolder ssc m) where
    getTxOut key = TxpLDHolder (asks utxoView) >>=
                   (atomically . STM.readTVar >=> UV.getTxOut key)

instance (MonadIO m, MonadUtxoRead ssc (TxpLDHolder ssc m))
       => MonadUtxo ssc (TxpLDHolder ssc m) where
    putTxOut key val = TxpLDHolder (asks utxoView) >>=
                       atomically . flip STM.modifyTVar' (UV.putTxOut key val)
    delTxIn key = TxpLDHolder (asks utxoView) >>=
                  atomically . flip STM.modifyTVar' (UV.delTxIn key)


runTxpLDHolder :: MonadIO m => TxpLDHolder ssc m a -> UtxoView ssc -> m a
runTxpLDHolder holder mp = TxpLDWrap
                       <$> liftIO (STM.newTVarIO mp)
                       <*> liftIO (STM.newTVarIO HS.empty)
                       >>= runReaderT (getTxpLDHolder holder)
