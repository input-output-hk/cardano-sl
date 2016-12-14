{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Pos.Modern.Txp.Class
       (
         MonadUtxoRead (..)
       , MonadUtxo (..)
       , MonadTxpLD (..)
       , TxpLD
       ) where

import           Universum

import           Pos.Modern.Txp.Storage.MemPool  (MemPool)
import           Pos.Modern.Txp.Storage.UtxoView (UtxoView)
import           Pos.Types                       (HeaderHash, TxIn, TxOut)

class Monad m => MonadUtxoRead ssc m | m -> ssc where
    getTxOut :: TxIn -> m (Maybe TxOut)

class MonadUtxoRead ssc m => MonadUtxo ssc m | m -> ssc where
    putTxOut :: TxIn -> TxOut -> m ()
    delTxIn :: TxIn -> m ()

type TxpLD ssc = (UtxoView ssc, MemPool, HeaderHash ssc)

class Monad m => MonadTxpLD ssc m | m -> ssc where
    getUtxoView :: m (UtxoView ssc)
    getMemPool  :: m MemPool
    setUtxoView :: UtxoView ssc -> m ()
    setMemPool  :: MemPool -> m ()
    modifyTxpLD :: (TxpLD ssc -> (a, TxpLD ssc)) -> m a
