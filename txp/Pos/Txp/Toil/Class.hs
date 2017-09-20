{-# LANGUAGE TypeFamilies #-}

-- | Type classes for Toil abstraction.
-- * MonadUtxoRead and MonadUtxo for encapsulation of Utxo storage.
-- * MonadStakesRead and MonadStakes for encapsulation of Stakes storage.
-- * MonadTxPoll for encapsulation of mem pool of local transactions.

module Pos.Txp.Toil.Class
       ( MonadUtxoRead (..)
       , utxoGetReader
       , MonadUtxo (..)
       , MonadStakesRead (..)
       , MonadStakes (..)
       , MonadTxPool (..)
       ) where

import           Universum

import           Control.Lens               (at, (.=))
import           Control.Monad.Trans.Class  (MonadTrans)
import qualified Ether
import           Serokell.Data.Memory.Units (Byte)

import           Pos.Core                   (Coin, StakeholderId, HasConfiguration)
import           Pos.Txp.Core.Types         (TxAux, TxId, TxIn, TxOutAux, TxUndo)
import           Pos.Txp.Toil.Types         (Utxo)
import           Pos.Util.Util              (HasLens', ether, lensOf)

----------------------------------------------------------------------------
-- MonadUtxo
----------------------------------------------------------------------------

class (HasConfiguration, Monad m) => MonadUtxoRead m where
    utxoGet :: TxIn -> m (Maybe TxOutAux)
    default utxoGet :: (MonadTrans t, MonadUtxoRead m', t m' ~ m) => TxIn -> m (Maybe TxOutAux)
    utxoGet = lift . utxoGet

instance {-# OVERLAPPABLE #-}
    (MonadUtxoRead m, MonadTrans t, Monad (t m)) =>
        MonadUtxoRead (t m)

-- | Implementation of 'utxoGet' which takes data from 'MonadReader' context.
utxoGetReader :: (HasLens' ctx Utxo, MonadReader ctx m) => TxIn -> m (Maybe TxOutAux)
utxoGetReader txIn = view $ lensOf @Utxo . at txIn

instance HasConfiguration => MonadUtxoRead ((->) Utxo) where
    utxoGet = utxoGetReader

instance (HasConfiguration, Monad m) => MonadUtxoRead (Ether.StateT' Utxo m) where
    utxoGet id = ether $ use (at id)

class MonadUtxoRead m => MonadUtxo m where
    utxoPut :: TxIn -> TxOutAux -> m ()
    default utxoPut :: (MonadTrans t, MonadUtxo m', t m' ~ m) => TxIn -> TxOutAux -> m ()
    utxoPut a = lift . utxoPut a
    utxoDel :: TxIn -> m ()
    default utxoDel :: (MonadTrans t, MonadUtxo m', t m' ~ m) => TxIn -> m ()
    utxoDel = lift . utxoDel

instance {-# OVERLAPPABLE #-}
  (MonadUtxo m, MonadTrans t, Monad (t m)) => MonadUtxo (t m)

instance (HasConfiguration, Monad m) => MonadUtxo (Ether.StateT' Utxo m) where
    utxoPut id v = ether $ at id .= Just v
    utxoDel id = ether $ at id .= Nothing

----------------------------------------------------------------------------
-- MonadStakes
----------------------------------------------------------------------------

class Monad m => MonadStakesRead m where
    getStake :: StakeholderId -> m (Maybe Coin)
    getTotalStake :: m Coin

instance {-# OVERLAPPABLE #-}
    (MonadStakesRead m, MonadTrans t, Monad (t m)) =>
        MonadStakesRead (t m)
  where
    getStake = lift . getStake
    getTotalStake = lift getTotalStake

class MonadStakesRead m => MonadStakes m where
    setStake :: StakeholderId -> Coin -> m ()
    setTotalStake :: Coin -> m ()

instance {-# OVERLAPPABLE #-}
    (MonadStakes m, MonadTrans t, Monad (t m)) =>
        MonadStakes (t m)
  where
    setStake id = lift . setStake id
    setTotalStake = lift . setTotalStake

----------------------------------------------------------------------------
-- MonadTxPool
----------------------------------------------------------------------------

class Monad m => MonadTxPool m where
    -- | Check whether Tx with given identifier is stored in the pool.
    hasTx :: TxId -> m Bool
    -- | Return size of the pool's binary representation (approximate).
    poolSize :: m Byte
    -- | Put a transaction with corresponding Undo into MemPool.
    -- Transaction must not be in MemPool.
    putTxWithUndo :: TxId -> TxAux -> TxUndo -> m ()

    default hasTx
        :: (MonadTrans t, MonadTxPool m', t m' ~ m) => TxId -> m Bool
    hasTx = lift . hasTx

    default poolSize
        :: (MonadTrans t, MonadTxPool m', t m' ~ m) => m Byte
    poolSize = lift poolSize

    default putTxWithUndo
        :: (MonadTrans t, MonadTxPool m', t m' ~ m) => TxId -> TxAux -> TxUndo -> m ()
    putTxWithUndo id tx = lift . putTxWithUndo id tx

instance {-# OVERLAPPABLE #-}
    (MonadTxPool m, MonadTrans t, Monad (t m)) =>
        MonadTxPool (t m)
