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
       ) where

import           Control.Lens         (at, use, view)
import           Control.Monad.Reader (runReaderT)
import           Universum

import           Pos.Types.Types      (TxIn (..), Utxo)
import           Pos.Types.Utxo.Class (MonadUtxoRead (..))

----------------------------------------------------------------------------
-- Reader
----------------------------------------------------------------------------

newtype UtxoReaderT m a = UtxoReaderT
    { getUtxoReaderT :: ReaderT Utxo m a
    } deriving (Functor, Applicative, Monad)

instance Monad m => MonadUtxoRead (UtxoReaderT m) where
    utxoGet TxIn {..} = UtxoReaderT $ view $ at (txInHash, txInIndex)

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
    } deriving (Functor, Applicative, Monad)

instance Monad m => MonadUtxoRead (UtxoStateT m) where
    utxoGet TxIn {..} = UtxoStateT $ use $ at (txInHash, txInIndex)

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
