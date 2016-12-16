{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Pos.Modern.Txp.Class
       (
         MonadTxpLD (..)
       , TxpLD
       ) where

import           Universum

import           Control.Monad.Trans          (MonadTrans)
import           Pos.Modern.Txp.Storage.Types (MemPool, UtxoView)
import           Pos.Types                    (HeaderHash)

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
type TxpLD ssc = (UtxoView ssc, MemPool, HeaderHash ssc)

class Monad m => MonadTxpLD ssc m | m -> ssc where
    getUtxoView :: m (UtxoView ssc)
    getMemPool  :: m MemPool
    setUtxoView :: UtxoView ssc -> m ()
    setMemPool  :: MemPool -> m ()
    modifyTxpLD :: (TxpLD ssc -> (a, TxpLD ssc)) -> m a
    modifyTxpLD_ :: (TxpLD ssc -> TxpLD ssc) -> m ()
    modifyTxpLD_ = modifyTxpLD . (((),) .)
    setTxpLD :: (TxpLD ssc) -> m ()
    setTxpLD txpLD = modifyTxpLD_ $ const txpLD

    default getUtxoView :: MonadTrans t => t m (UtxoView ssc)
    getUtxoView = lift  getUtxoView

    default setUtxoView :: MonadTrans t => UtxoView ssc -> t m ()
    setUtxoView = lift . setUtxoView

    default getMemPool :: MonadTrans t => t m MemPool
    getMemPool = lift getMemPool

    default setMemPool :: MonadTrans t => MemPool -> t m ()
    setMemPool  = lift . setMemPool

    default modifyTxpLD :: MonadTrans t => (TxpLD ssc -> (a, TxpLD ssc)) -> t m a
    modifyTxpLD = lift . modifyTxpLD

instance MonadTxpLD ssc m => MonadTxpLD ssc (ReaderT r m)
