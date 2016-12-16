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

    default setUtxoView :: MonadTrans t =>UtxoView ssc -> t m ()
    setUtxoView = lift . setUtxoView

    default getMemPool :: MonadTrans t => t m MemPool
    getMemPool = lift getMemPool

    default setMemPool :: MonadTrans t => MemPool -> t m ()
    setMemPool  = lift . setMemPool

    default modifyTxpLD :: MonadTrans t => (TxpLD ssc -> (a, TxpLD ssc)) -> t m a
    modifyTxpLD = lift . modifyTxpLD

instance MonadTxpLD ssc m => MonadTxpLD ssc (ReaderT r m)
