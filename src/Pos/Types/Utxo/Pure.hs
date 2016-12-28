{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}

-- | Pure version of UTXO.

module Pos.Types.Utxo.Pure
       ( UtxoReaderT (..)
       , runUtxoReaderT

       , UtxoReader
       , runUtxoReader

       , UtxoStateT (..)
       , runUtxoStateT
       , evalUtxoStateT
       , execUtxoStateT

       , UtxoState
       , runUtxoState
       , evalUtxoState
       , execUtxoState

       , applyTxToUtxoPure
       , applyTxToUtxoPure'
       , verifyTxUtxoPure
       ) where

import           Control.Lens             (at, use, view, (.=))
import           Control.Monad.Reader     (runReaderT)
import           Control.Monad.Trans      (MonadTrans (..))
import           Serokell.Util.Verify     (VerificationRes (..))
import           Universum

import           Pos.Binary.Types         ()
import           Pos.Crypto               (WithHash (..))
import           Pos.Types.Types          (Tx, TxAux, TxDistribution, TxId, TxIn (..),
                                           Utxo)
import           Pos.Types.Utxo.Class     (MonadUtxo (..), MonadUtxoRead (..))
import           Pos.Types.Utxo.Functions (applyTxToUtxo, applyTxToUtxo', verifyTxUtxo)
import           Pos.Util                 (eitherToVerRes)
----------------------------------------------------------------------------
-- Reader
----------------------------------------------------------------------------

newtype UtxoReaderT m a = UtxoReaderT
    { getUtxoReaderT :: ReaderT Utxo m a
    } deriving (Functor, Applicative, Monad, MonadReader Utxo)

instance Monad m => MonadUtxoRead (UtxoReaderT m) where
    utxoGet TxIn {..} = UtxoReaderT $ view $ at (txInHash, txInIndex)

instance MonadTrans UtxoReaderT where
    lift = UtxoReaderT . lift

runUtxoReaderT :: UtxoReaderT m a -> Utxo -> m a
runUtxoReaderT = runReaderT . getUtxoReaderT

type UtxoReader = UtxoReaderT Identity

runUtxoReader :: UtxoReader a -> Utxo -> a
runUtxoReader r = runIdentity . runUtxoReaderT r

----------------------------------------------------------------------------
-- State
----------------------------------------------------------------------------

newtype UtxoStateT m a = UtxoStateT
    { getUtxoStateT :: StateT Utxo m a
    } deriving (Functor, Applicative, Monad, MonadState Utxo)

instance Monad m => MonadUtxoRead (UtxoStateT m) where
    utxoGet TxIn {..} = UtxoStateT $ use $ at (txInHash, txInIndex)

instance Monad m => MonadUtxo (UtxoStateT m) where
    utxoPut TxIn {..} v = UtxoStateT $ at (txInHash, txInIndex) .= Just v
    utxoDel TxIn {..} = UtxoStateT $ at (txInHash, txInIndex) .= Nothing

instance MonadTrans UtxoStateT where
    lift = UtxoStateT . lift

runUtxoStateT :: UtxoStateT m a -> Utxo -> m (a, Utxo)
runUtxoStateT = runStateT . getUtxoStateT

evalUtxoStateT :: Monad m => UtxoStateT m a -> Utxo -> m a
evalUtxoStateT = evalStateT . getUtxoStateT

execUtxoStateT :: Monad m => UtxoStateT m a -> Utxo -> m Utxo
execUtxoStateT = execStateT . getUtxoStateT

type UtxoState = UtxoStateT Identity

runUtxoState :: UtxoState a -> Utxo -> (a, Utxo)
runUtxoState r = runIdentity . runUtxoStateT r

evalUtxoState :: UtxoState a -> Utxo -> a
evalUtxoState r = runIdentity . evalUtxoStateT r

execUtxoState :: UtxoState a -> Utxo -> Utxo
execUtxoState r = runIdentity . execUtxoStateT r

----------------------------------------------------------------------------
-- Pure versions of functions
----------------------------------------------------------------------------

-- | Pure version of applyTxToUtxo.
applyTxToUtxoPure :: WithHash Tx -> TxDistribution -> Utxo -> Utxo
applyTxToUtxoPure tx d = execUtxoState $ applyTxToUtxo tx d

-- | Pure version of applyTxToUtxo'.
applyTxToUtxoPure' :: (TxId, TxAux) -> Utxo -> Utxo
applyTxToUtxoPure' w = execUtxoState $ applyTxToUtxo' w

-- CHECK: @TxUtxoPure
-- #verifyTxUtxo

-- | Pure version of verifyTxUtxo.
verifyTxUtxoPure :: Bool -> Utxo -> TxAux -> VerificationRes
verifyTxUtxoPure verifyAlone utxo txw =
    eitherToVerRes $ runUtxoReader (verifyTxUtxo verifyAlone txw) utxo
