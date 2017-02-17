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

    default utxoGet :: (MonadTrans t, MonadTxpRead m', t m' ~ m) => TxIn -> m (Maybe TxOutAux)
    utxoGet = lift . utxoGet

class MonadTxpRead m => MonadTxp m where
    utxoPut :: TxIn -> TxOutAux -> m ()
    utxoDel :: TxIn -> m ()
    setStake :: StakeholderId -> Coin -> m ()
    setTotalStake :: Coin -> m ()

    default utxoPut :: (MonadTrans t, MonadTxp m', t m' ~ m) => TxIn -> TxOutAux -> m ()
    utxoPut a = lift . utxoPut a

    default utxoDel :: (MonadTrans t, MonadTxp m', t m' ~ m) => TxIn -> m ()
    utxoDel = lift . utxoDel
