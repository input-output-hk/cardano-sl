-- | Pure version of UTXO.

module Pos.Types.Utxo.Pure
       ( UtxoReaderT (..)
       , runUtxoReaderT

       , UtxoReader
       , runUtxoReader
       ) where

import           Control.Lens         (at, view)
import           Control.Monad.Reader (runReaderT)
import           Universum

import           Pos.Types.Types      (TxIn (..), Utxo)
import           Pos.Types.Utxo.Class (MonadUtxoRead (..))

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
