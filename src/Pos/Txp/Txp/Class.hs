{-# LANGUAGE TypeFamilies #-}

module Pos.Txp.Txp.Class
      ( MonadUtxoRead (..)
      , MonadUtxo (..)
      , MonadBalancesRead (..)
      , MonadBalances (..)
      , MonadTxPool (..)
      ) where

import           Control.Monad.Trans.Class (MonadTrans)
import           Universum

import           Pos.Types                 (TxAux, TxId, TxIn, TxOutAux, TxUndo, StakeholderId, Coin)

----------------------------------------------------------------------------
-- MonadUtxo
----------------------------------------------------------------------------

class Monad m => MonadUtxoRead m where
    utxoGet :: TxIn -> m (Maybe TxOutAux)
    default utxoGet :: (MonadTrans t, MonadUtxoRead m', t m' ~ m) => TxIn -> m (Maybe TxOutAux)
    utxoGet = lift . utxoGet

instance MonadUtxoRead m => MonadUtxoRead (ReaderT a m) where
instance MonadUtxoRead m => MonadUtxoRead (ExceptT e m) where
instance MonadUtxoRead m => MonadUtxoRead (StateT e m) where
-- For pure runs
instance MonadUtxoRead Identity where
    utxoGet _ = pure Nothing

class MonadUtxoRead m => MonadUtxo m where
    utxoPut :: TxIn -> TxOutAux -> m ()
    default utxoPut :: (MonadTrans t, MonadUtxo m', t m' ~ m) => TxIn -> TxOutAux -> m ()
    utxoPut a = lift . utxoPut a
    utxoDel :: TxIn -> m ()
    default utxoDel :: (MonadTrans t, MonadUtxo m', t m' ~ m) => TxIn -> m ()
    utxoDel = lift . utxoDel

instance MonadUtxo m => MonadUtxo (ReaderT e m) where
instance MonadUtxo m => MonadUtxo (ExceptT e m) where
instance MonadUtxo m => MonadUtxo (StateT e m) where

----------------------------------------------------------------------------
-- MonadBalances
----------------------------------------------------------------------------

class Monad m => MonadBalancesRead m where
    getStake :: StakeholderId -> m (Maybe Coin)
    getTotalStake :: m Coin

    default getStake
        :: (MonadTrans t, MonadBalancesRead m', t m' ~ m) => StakeholderId -> m (Maybe Coin)
    getStake = lift . getStake

    default getTotalStake
        :: (MonadTrans t, MonadBalancesRead m', t m' ~ m) => m Coin
    getTotalStake = lift getTotalStake


instance MonadBalancesRead m => MonadBalancesRead (ReaderT s m)
instance MonadBalancesRead m => MonadBalancesRead (StateT s m)
instance MonadBalancesRead m => MonadBalancesRead (ExceptT s m)

class MonadBalancesRead m => MonadBalances m where
    setStake :: StakeholderId -> Coin -> m ()
    setTotalStake :: Coin -> m ()

    default setStake
        :: (MonadTrans t, MonadBalances m', t m' ~ m) => StakeholderId -> Coin -> m ()
    setStake id = lift . setStake id

    default setTotalStake
        :: (MonadTrans t, MonadBalances m', t m' ~ m) => Coin -> m ()
    setTotalStake = lift . setTotalStake

instance MonadBalances m => MonadBalances (ReaderT s m)
instance MonadBalances m => MonadBalances (StateT s m)
instance MonadBalances m => MonadBalances (ExceptT s m)

----------------------------------------------------------------------------
-- MonadTx
----------------------------------------------------------------------------

class Monad m => MonadTxPool m where
    hasTx :: TxId -> m Bool
    putTxWithUndo :: TxId -> TxAux -> TxUndo -> m ()
    poolSize :: m Int

    default hasTx
        :: (MonadTrans t, MonadTxPool m', t m' ~ m) => TxId -> m Bool
    hasTx = lift . hasTx

    default putTxWithUndo
        :: (MonadTrans t, MonadTxPool m', t m' ~ m) => TxId -> TxAux -> TxUndo -> m ()
    putTxWithUndo id tx = lift . putTxWithUndo id tx

    default poolSize
        :: (MonadTrans t, MonadTxPool m', t m' ~ m) => m Int
    poolSize = lift poolSize

instance MonadTxPool m => MonadTxPool (ReaderT s m)
instance MonadTxPool m => MonadTxPool (StateT s m)
instance MonadTxPool m => MonadTxPool (ExceptT s m)
