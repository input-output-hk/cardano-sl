{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Pos.Txp.Class
       (
         MonadTxpLD (..)
       , TxpLD
       , getLocalTxs
       , getLocalUndo
       , getLocalTxsNUndo
       , getUtxoView
       , getMemPool
       ) where

import           Control.Monad.Trans (MonadTrans)
import qualified Data.HashMap.Strict as HM
import           Universum

import           Pos.DHT.Model.Class (DHTResponseT)
import           Pos.DHT.Real        (KademliaDHT)
import           Pos.Txp.Types       (MemPool (localTxs), UtxoView)
import           Pos.Types           (HeaderHash, TxAux, TxId, TxOutAux)

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

class Monad m => MonadTxpLD ssc m | m -> ssc where
    setUtxoView  :: UtxoView ssc -> m ()
    setMemPool   :: MemPool -> m ()
    modifyTxpLD  :: (TxpLD ssc -> (a, TxpLD ssc)) -> m a
    modifyTxpLD_ :: (TxpLD ssc -> TxpLD ssc) -> m ()
    modifyTxpLD_ = modifyTxpLD . (((),) .)
    getTxpLD     :: m (TxpLD ssc)
    setTxpLD     :: TxpLD ssc -> m ()
    setTxpLD txpLD = modifyTxpLD_ $ const txpLD

    -- default getUtxoView :: MonadTrans t => t m (UtxoView ssc)
    -- getUtxoView = lift  getUtxoView

    default setUtxoView :: MonadTrans t => UtxoView ssc -> t m ()
    setUtxoView = lift . setUtxoView

    default setMemPool :: MonadTrans t => MemPool -> t m ()
    setMemPool  = lift . setMemPool

    default modifyTxpLD :: MonadTrans t => (TxpLD ssc -> (a, TxpLD ssc)) -> t m a
    modifyTxpLD = lift . modifyTxpLD

    default getTxpLD :: MonadTrans t => t m (TxpLD ssc)
    getTxpLD = lift getTxpLD

instance MonadTxpLD ssc m => MonadTxpLD ssc (ReaderT r m)

instance MonadTxpLD ssc m => MonadTxpLD ssc (DHTResponseT s m)

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
