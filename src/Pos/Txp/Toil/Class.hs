{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}

-- | Type classes for Toil abstraction.
-- * MonadUtxoRead and MonadUtxo for encapsulation of Utxo storage.
-- * MonadBalancesRead and MonadBalances for encapsulation of Balances storage.
-- * MonadTxPoll for encapsulation of mem pool of local transactions.

module Pos.Txp.Toil.Class
       ( MonadUtxoRead (..)
       , MonadUtxo (..)
       , MonadBalancesRead (..)
       , MonadBalances (..)
       , MonadTxPool (..)
#ifdef WITH_EXPLORER
       , MonadTxExtra (..)
       , MonadTxExtraRead (..)
#endif
       ) where

import           Control.Monad.Trans.Class (MonadTrans)
import           Universum

import           Pos.Core                  (Coin, StakeholderId)
import           Pos.Txp.Core.Types        (TxAux, TxId, TxIn, TxOutAux, TxUndo)
#ifdef WITH_EXPLORER
import           Pos.Types.Explorer        (TxExtra)
#endif

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
-- MonadTxPool
----------------------------------------------------------------------------

class Monad m => MonadTxPool m where
    hasTx :: TxId -> m Bool
    poolSize :: m Int
    putTxWithUndo :: TxId -> TxAux -> TxUndo -> m ()

    default hasTx
        :: (MonadTrans t, MonadTxPool m', t m' ~ m) => TxId -> m Bool
    hasTx = lift . hasTx

    default poolSize
        :: (MonadTrans t, MonadTxPool m', t m' ~ m) => m Int
    poolSize = lift poolSize

    default putTxWithUndo
        :: (MonadTrans t, MonadTxPool m', t m' ~ m) => TxId -> TxAux -> TxUndo -> m ()
    putTxWithUndo id tx = lift . putTxWithUndo id tx

instance MonadTxPool m => MonadTxPool (ReaderT s m)
instance MonadTxPool m => MonadTxPool (StateT s m)
instance MonadTxPool m => MonadTxPool (ExceptT s m)

#ifdef WITH_EXPLORER
----------------------------------------------------------------------------
-- MonadTxExtra
----------------------------------------------------------------------------

class Monad m => MonadTxExtraRead m where
    getTxExtra :: TxId -> m (Maybe TxExtra)

    default getTxExtra
        :: (MonadTrans t, MonadTxExtraRead m', t m' ~ m) => TxId -> m (Maybe TxExtra)
    getTxExtra = lift . getTxExtra

instance MonadTxExtraRead m => MonadTxExtraRead (ReaderT s m)
instance MonadTxExtraRead m => MonadTxExtraRead (StateT s m)
instance MonadTxExtraRead m => MonadTxExtraRead (ExceptT s m)

class MonadTxExtraRead m => MonadTxExtra m where
    putTxExtra :: TxId -> TxExtra -> m ()
    delTxExtra :: TxId -> m ()

    default putTxExtra
        :: (MonadTrans t, MonadTxExtra m', t m' ~ m) => TxId -> TxExtra -> m ()
    putTxExtra id = lift . putTxExtra id

    default delTxExtra
        :: (MonadTrans t, MonadTxExtra m', t m' ~ m) => TxId -> m ()
    delTxExtra = lift . delTxExtra

instance MonadTxExtra m => MonadTxExtra (ReaderT s m)
instance MonadTxExtra m => MonadTxExtra (StateT s m)
instance MonadTxExtra m => MonadTxExtra (ExceptT s m)
#endif
