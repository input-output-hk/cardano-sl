{-# LANGUAGE TypeFamilies #-}

-- | Type class abstracting UTXO (set of unspent outputs).

module Pos.Types.Utxo.Class
       ( MonadUtxoRead (..)
       , MonadUtxo (..)
       ) where

import           Control.Monad.Except (ExceptT)
import           Control.Monad.Trans  (MonadTrans)
import           Control.TimeWarp.Rpc (ResponseT)
import           Universum

import           Pos.DHT.Model.Class  (DHTResponseT)
import           Pos.DHT.Real         (KademliaDHT)
import           Pos.Types.Types      (TxIn, TxOutAux)

class Monad m => MonadUtxoRead m where
    utxoGet :: TxIn -> m (Maybe TxOutAux)
    default utxoGet :: (MonadTrans t, MonadUtxoRead m', t m' ~ m) => TxIn -> t m' (Maybe TxOutAux)
    utxoGet = lift . utxoGet

class MonadUtxoRead m => MonadUtxo m where
    utxoPut :: TxIn -> TxOutAux -> m ()
    default utxoPut :: (MonadTrans t, MonadUtxo m', t m' ~ m) => TxIn -> TxOutAux -> t m' ()
    utxoPut a = lift . utxoPut a
    utxoDel :: TxIn -> m ()
    default utxoDel :: (MonadTrans t, MonadUtxo m', t m' ~ m) => TxIn -> t m' ()
    utxoDel = lift . utxoDel

instance MonadUtxoRead m => MonadUtxoRead (ReaderT a m) where
instance MonadUtxo m => MonadUtxo (ReaderT e m) where

instance MonadUtxoRead m => MonadUtxoRead (ExceptT e m) where
instance MonadUtxo m => MonadUtxo (ExceptT e m) where

instance MonadUtxoRead m => MonadUtxoRead (ResponseT s m) where
instance MonadUtxo m => MonadUtxo (ResponseT e m) where

instance MonadUtxoRead m => MonadUtxoRead (DHTResponseT s m) where
instance MonadUtxo m => MonadUtxo (DHTResponseT s m) where

instance MonadUtxoRead m => MonadUtxoRead (KademliaDHT m) where
instance MonadUtxo m => MonadUtxo (KademliaDHT m) where
