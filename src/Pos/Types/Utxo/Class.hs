-- | Type class abstracting UTXO (set of unspent outputs).

module Pos.Types.Utxo.Class
       ( MonadUtxoRead (..)
       , MonadUtxo (..)
       ) where

import           Control.Monad.Except (ExceptT)
import           Universum

import           Pos.Types.Types      (TxIn, TxOut)

class Monad m => MonadUtxoRead m where
    utxoGet :: TxIn -> m (Maybe TxOut)

class MonadUtxoRead m => MonadUtxo m where
    utxoPut :: TxIn -> TxOut -> m ()
    utxoDel :: TxIn -> m ()

instance MonadUtxoRead m => MonadUtxoRead (ReaderT e m) where
    utxoGet = lift . utxoGet

instance MonadUtxo m => MonadUtxo (ReaderT e m) where
    utxoPut a = lift . utxoPut a
    utxoDel = lift . utxoDel

instance MonadUtxoRead m => MonadUtxoRead (ExceptT e m) where
    utxoGet = lift . utxoGet

instance MonadUtxo m => MonadUtxo (ExceptT e m) where
    utxoPut a = lift . utxoPut a
    utxoDel = lift . utxoDel
