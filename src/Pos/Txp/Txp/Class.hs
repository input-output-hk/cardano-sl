{-# LANGUAGE TypeFamilies #-}

module Pos.Txp.Txp.Class
      ( MonadTxpRead (..)
      , MonadTxp (..)
      ) where

import           Control.Monad.Trans.Class (MonadTrans)
import           System.Wlog               (WithLogger)
import           Universum

import           Pos.Types.Core
import           Pos.Types.Types

class Monad m => MonadTxpRead m where
    utxoGet :: TxIn -> m (Maybe TxOutAux)
    getStake :: StakeholderId -> m (Maybe Coin)
    getTotalStake :: m Coin
    hasTx :: m Bool

    default utxoGet
        :: (MonadTrans t, MonadTxpRead m', t m' ~ m) => TxIn -> m (Maybe TxOutAux)
    utxoGet = lift . utxoGet

    default getStake
        :: (MonadTrans t, MonadTxpRead m', t m' ~ m) => StakeholderId -> m (Maybe Coin)
    getStake = lift . getStake

    default getTotalStake
        :: (MonadTrans t, MonadTxpRead m', t m' ~ m) => m Coin
    getTotalStake = lift getTotalStake

    default hasTx
        :: (MonadTrans t, MonadTxpRead m', t m' ~ m) => m Bool
    hasTx = lift hasTx

instance MonadTxpRead m => MonadTxpRead (ReaderT s m)
instance MonadTxpRead m => MonadTxpRead (StateT s m)
instance MonadTxpRead m => MonadTxpRead (ExceptT s m)

class MonadTxpRead m => MonadTxp m where
    utxoPut :: TxIn -> TxOutAux -> m ()
    utxoDel :: TxIn -> m ()
    setStake :: StakeholderId -> Coin -> m ()
    setTotalStake :: Coin -> m ()

    default utxoPut :: (MonadTrans t, MonadTxp m', t m' ~ m) => TxIn -> TxOutAux -> m ()
    utxoPut a = lift . utxoPut a

    default utxoDel :: (MonadTrans t, MonadTxp m', t m' ~ m) => TxIn -> m ()
    utxoDel = lift . utxoDel

    default setStake
        :: (MonadTrans t, MonadTxp m', t m' ~ m) => StakeholderId -> Coin -> m ()
    setStake id = lift . setStake id

    default setTotalStake
        :: (MonadTrans t, MonadTxp m', t m' ~ m) => Coin -> m ()
    setTotalStake = lift . setTotalStake

instance MonadTxp m => MonadTxp (ReaderT s m)
instance MonadTxp m => MonadTxp (StateT s m)
instance MonadTxp m => MonadTxp (ExceptT s m)
