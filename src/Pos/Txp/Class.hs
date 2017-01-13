{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Pos.Txp.Class
       ( TxpLDWrap (..)
       , MonadTxpLD (..)
       , TxpLD
       , getLocalTxs
       , getLocalUndo
       , getLocalTxsNUndo
       , getUtxoView
       , getMemPool
       ) where

import qualified Control.Concurrent.STM as STM
import           Control.Monad.Except   (ExceptT)
import           Control.Monad.Trans    (MonadTrans)
import qualified Data.HashMap.Strict    as HM
import           Universum

import           Pos.DHT.Real           (KademliaDHT)
import           Pos.Txp.Types.Types    (MemPool (localTxs), UtxoView)
import           Pos.Types              (HeaderHash, TxAux, TxId, TxOutAux)

-- | LocalData of transactions processing.
-- There are two invariants which must hold for local data
-- (where uv is UtxoView, memPool is MemPool and tip is HeaderHash):
-- 1. Suppose 'blks' is sequence of blocks from the very beggining up
-- to 'tip'. If one applies 'blks' to genesis Utxo, resulting Utxo
-- (let's call it 'utxo1') will be such that all transactions from
-- 'memPool' are valid with respect to it.
-- 2. If one applies all transactions from 'memPool' to 'utxo1',
-- resulting Utxo will be equivalent to 'uv' with respect to
-- MonadUtxo.
type TxpLD ssc = (UtxoView ssc, MemPool, HashMap TxId [TxOutAux], HeaderHash ssc)

-- | Real data inside TxpLDHolder
data TxpLDWrap ssc = TxpLDWrap
    { utxoView :: !(STM.TVar (UtxoView ssc))
    , memPool  :: !(STM.TVar MemPool)
    , undos    :: !(STM.TVar (HashMap TxId [TxOutAux]))
    , ldTip    :: !(STM.TVar (HeaderHash ssc))
    }

-- TODO: this monad class is stupid and should be removed. Method
-- `getTxpLDWrap` returns inside implementation and it's needed in
-- Pos.Web.Server. So practically all other methods of these class can
-- be implemented using 'getTxpLDWrap', so this class is just
-- MonadReader instantiated to 'TxpLDWrap'.
class Monad m => MonadTxpLD ssc m | m -> ssc where
    getTxpLDWrap :: m (TxpLDWrap ssc)
    setUtxoView  :: UtxoView ssc -> m ()
    setMemPool   :: MemPool -> m ()
    modifyTxpLD  :: (TxpLD ssc -> (a, TxpLD ssc)) -> m a
    modifyTxpLD_ :: (TxpLD ssc -> TxpLD ssc) -> m ()
    modifyTxpLD_ = modifyTxpLD . (((),) .)
    getTxpLD     :: m (TxpLD ssc)
    setTxpLD     :: TxpLD ssc -> m ()
    setTxpLD txpLD = modifyTxpLD_ $ const txpLD

    default getTxpLDWrap :: (MonadTrans t, MonadTxpLD ssc m', t m' ~ m) => m (TxpLDWrap ssc)
    getTxpLDWrap = lift getTxpLDWrap

    default setUtxoView :: (MonadTrans t, MonadTxpLD ssc m', t m' ~ m) => UtxoView ssc -> m ()
    setUtxoView = lift . setUtxoView

    default setMemPool :: (MonadTrans t, MonadTxpLD ssc m', t m' ~ m) => MemPool -> m ()
    setMemPool  = lift . setMemPool

    default modifyTxpLD :: (MonadTrans t, MonadTxpLD ssc m', t m' ~ m) => (TxpLD ssc -> (a, TxpLD ssc)) -> m a
    modifyTxpLD = lift . modifyTxpLD

    default getTxpLD :: (MonadTrans t, MonadTxpLD ssc m', t m' ~ m) => m (TxpLD ssc)
    getTxpLD = lift getTxpLD

instance MonadTxpLD ssc m => MonadTxpLD ssc (ReaderT r m)
instance MonadTxpLD ssc m => MonadTxpLD ssc (ExceptT r m)
instance MonadTxpLD ssc m => MonadTxpLD ssc (KademliaDHT m)

getLocalTxs :: MonadTxpLD ssc m => m [(TxId, TxAux)]
getLocalTxs = HM.toList . localTxs <$> getMemPool

getLocalUndo :: MonadTxpLD ssc m => m (HashMap TxId [TxOutAux])
getLocalUndo = (\(_, _, undos, _) -> undos) <$> getTxpLD

getLocalTxsNUndo :: MonadTxpLD ssc m => m ([(TxId, TxAux)], HashMap TxId [TxOutAux])
getLocalTxsNUndo = (\(_, mp, undos, _) -> (HM.toList . localTxs $ mp, undos))
                <$> getTxpLD

getUtxoView :: MonadTxpLD ssc m => m (UtxoView ssc)
getUtxoView = (\(uv, _, _, _) -> uv) <$> getTxpLD

getMemPool :: MonadTxpLD ssc m => m MemPool
getMemPool = (\(_, mp, _, _) -> mp) <$> getTxpLD
