{-# LANGUAGE TypeFamilies #-}

-- | Type classes for Toil abstraction.
-- * MonadUtxoRead and MonadUtxo for encapsulation of Utxo storage.
-- * MonadStakesRead and MonadStakes for encapsulation of Stakes storage.
-- * MonadTxPoll for encapsulation of mem pool of local transactions.

module Pos.Txp.Toil.Class
       ( MonadUtxoRead (..)
       , utxoGetReader
       , MonadUtxo (..)
       , utxoPut
       , utxoDel
       , MonadStakesRead (..)
       , MonadStakes (..)
       , MonadTxPool (..)
       ) where

import           Universum

import           Control.Lens (at, (.=))
import           Control.Monad.Trans.Class (MonadTrans)
import qualified Ether
import           Fmt ((+|), (|+))

import           Pos.Core (Coin, HasConfiguration, StakeholderId)
import           Pos.Core.Txp (TxAux, TxId, TxIn, TxOutAux, TxUndo)
import           Pos.Txp.Toil.Types (Utxo)
import           Pos.Util.Util (HasLens', ether, lensOf)

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
    -- | Add an unspent output to UTXO.
    utxoPutUnchecked :: TxIn -> TxOutAux -> m ()
    default utxoPutUnchecked
        :: (MonadTrans t, MonadUtxo m', t m' ~ m) => TxIn -> TxOutAux -> m ()
    utxoPutUnchecked a = lift . utxoPutUnchecked a

    -- | Delete an unspent input from UTXO.
    utxoDelUnchecked :: TxIn -> m ()
    default utxoDelUnchecked
        :: (MonadTrans t, MonadUtxo m', t m' ~ m) => TxIn -> m ()
    utxoDelUnchecked = lift . utxoDelUnchecked

instance {-# OVERLAPPABLE #-}
  (MonadUtxo m, MonadTrans t, Monad (t m)) => MonadUtxo (t m)

instance (HasConfiguration, Monad m) => MonadUtxo (Ether.StateT' Utxo m) where
    utxoPutUnchecked id aux = ether $ at id .= Just aux
    utxoDelUnchecked id     = ether $ at id .= Nothing

-- | Add an unspent output to UTXO. If it's already there, throw an 'error'.
utxoPut :: MonadUtxo m => TxIn -> TxOutAux -> m ()
utxoPut id aux = utxoGet id >>= \case
    Nothing -> utxoPutUnchecked id aux
    Just _  -> error ("utxoPut: "+|id|+" is already in utxo")

-- | Delete an unspent input from UTXO. If it's not there, throw an 'error'.
utxoDel :: MonadUtxo m => TxIn -> m ()
utxoDel id = utxoGet id >>= \case
    Just _  -> utxoDelUnchecked id
    Nothing -> error ("utxoDel: "+|id|+" is not in the utxo")

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
    -- | Return the number of transactions contained in the pool.
    poolSize :: m Int
    -- | Put a transaction with corresponding Undo into MemPool.
    -- Transaction must not be in MemPool.
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

instance {-# OVERLAPPABLE #-}
    (MonadTxPool m, MonadTrans t, Monad (t m)) =>
        MonadTxPool (t m)
