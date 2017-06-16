{-# LANGUAGE CPP #-}

-- | Pure version of UTXO.

module Pos.Txp.Toil.Utxo.Pure
       ( UtxoReaderT
       , runUtxoReaderT

       , UtxoReader
       , runUtxoReader

       , UtxoStateT
       , runUtxoStateT
       , evalUtxoStateT
       , execUtxoStateT

       , UtxoState
       , runUtxoState
       , evalUtxoState
       , execUtxoState

       , applyTxToUtxoPure
       , verifyTxUtxoPure
       ) where

import           Control.Monad.Except        (MonadError)
import qualified Ether
import           Universum

import           Pos.Binary.Core             ()
import           Pos.Crypto                  (WithHash (..))
import           Pos.Txp.Core                (Tx, TxAux, TxDistribution, TxUndo)
import           Pos.Txp.Toil.Failure        (ToilVerFailure)
import           Pos.Txp.Toil.Types          (Utxo)
import           Pos.Txp.Toil.Utxo.Functions (VTxContext, applyTxToUtxo, verifyTxUtxo)

----------------------------------------------------------------------------
-- Reader
----------------------------------------------------------------------------

type UtxoReaderT = Ether.ReaderT' Utxo

runUtxoReaderT :: UtxoReaderT m a -> Utxo -> m a
runUtxoReaderT = Ether.runReaderT

type UtxoReader = UtxoReaderT Identity

runUtxoReader :: UtxoReader a -> Utxo -> a
runUtxoReader = Ether.runReader

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

type UtxoState = UtxoStateT Identity

runUtxoState :: UtxoState a -> Utxo -> (a, Utxo)
runUtxoState = Ether.runState

evalUtxoState :: UtxoState a -> Utxo -> a
evalUtxoState = Ether.evalState

execUtxoState :: UtxoState a -> Utxo -> Utxo
execUtxoState = Ether.execState

----------------------------------------------------------------------------
-- Pure versions of functions
----------------------------------------------------------------------------

-- | Pure version of verifyTxUtxo.
verifyTxUtxoPure
    :: MonadError ToilVerFailure m
    => VTxContext -> Utxo -> TxAux -> m TxUndo
verifyTxUtxoPure ctx utxo txAux = runUtxoReaderT (verifyTxUtxo ctx txAux) utxo

-- | Pure version of applyTxToUtxo.
applyTxToUtxoPure :: WithHash Tx -> TxDistribution -> Utxo -> Utxo
applyTxToUtxoPure tx d = execUtxoState $ applyTxToUtxo tx d
