{-# LANGUAGE CPP #-}

-- | Pure version of UTXO.

module Pos.Txp.Toil.Utxo.Pure
       ( UtxoStateT
       , runUtxoStateT
       , evalUtxoStateT
       , execUtxoStateT

       , applyTxToUtxoPure
       , verifyTxUtxoPure
       ) where

import           Control.Monad.Except        (MonadError)
import           Data.Functor.Identity       (runIdentity)
import qualified Ether
import           Universum

import           Pos.Binary.Core             ()
import           Pos.Crypto                  (WithHash (..))
import           Pos.Txp.Core                (Tx, TxAux, TxDistribution, TxUndo)
import           Pos.Txp.Toil.Failure        (ToilVerFailure)
import           Pos.Txp.Toil.Types          (TxFee, Utxo)
import           Pos.Txp.Toil.Utxo.Functions (VTxContext, applyTxToUtxo, verifyTxUtxo)

----------------------------------------------------------------------------
-- State
----------------------------------------------------------------------------

type UtxoStateT = Ether.StateT' Utxo

runUtxoStateT :: UtxoStateT m a -> Utxo -> m (a, Utxo)
runUtxoStateT = Ether.runStateT

evalUtxoStateT :: Monad m => UtxoStateT m a -> Utxo -> m a
evalUtxoStateT = Ether.evalStateT

execUtxoStateT :: Monad m => UtxoStateT m a -> Utxo -> m Utxo
execUtxoStateT = Ether.execStateT

----------------------------------------------------------------------------
-- Pure versions of functions
----------------------------------------------------------------------------

-- | Pure version of verifyTxUtxo.
verifyTxUtxoPure
    :: MonadError ToilVerFailure m
    => VTxContext -> Utxo -> TxAux -> m (TxUndo, TxFee)
verifyTxUtxoPure ctx utxo txAux = evalUtxoStateT (verifyTxUtxo ctx txAux) utxo

-- | Pure version of applyTxToUtxo.
applyTxToUtxoPure :: WithHash Tx -> TxDistribution -> Utxo -> Utxo
applyTxToUtxoPure tx d u = runIdentity $ execUtxoStateT (applyTxToUtxo tx d) u
