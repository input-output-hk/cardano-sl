{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Pos.Modern.Txp.Storage.Types
       (
         MonadUtxoRead (..)
       , MonadUtxo (..)
       , runMPHolder
       ) where
import qualified Control.Concurrent.STM         as STM
import           Control.Monad.Reader           (ReaderT (ReaderT))
import           Universum

import           Pos.Modern.Txp.Storage.MemPool (MemPool)
import qualified Pos.Modern.Txp.Storage.MemPool as MP
import           Pos.Types                      (TxId, TxIn (..), TxOut)

class Monad m => MonadUtxoRead ssc m | m -> ssc where
    getTxOut :: (TxId, Word32) -> m (Maybe TxOut)

class MonadUtxoRead ssc m => MonadUtxo ssc m | m -> ssc where
    putTxOut :: (TxId, Word32) -> TxOut -> m ()
    delTxIn :: TxIn -> m ()

newtype MemPoolHolder ssc m a = MemPoolHolder
    { getMemPoolHolder :: ReaderT (STM.TVar (MemPool ssc)) m a
    } deriving (Functor, Applicative, Monad, MonadIO)

instance MonadIO m => MonadUtxoRead ssc (MemPoolHolder ssc m) where
    getTxOut key = MemPoolHolder ask >>= (atomically . STM.readTVar >=> MP.getTxOut key)

instance (MonadIO m, MonadUtxoRead ssc (MemPoolHolder ssc m))
       => MonadUtxo ssc (MemPoolHolder ssc m) where
    putTxOut key val = MemPoolHolder ask >>= atomically . flip STM.modifyTVar' (MP.putTxOut key val)
    delTxIn key = MemPoolHolder ask >>= atomically . flip STM.modifyTVar' (MP.delTxIn key)

runMPHolder :: MonadIO m => MemPool ssc -> MemPoolHolder ssc m a -> m a
runMPHolder mp holder = liftIO (STM.newTVarIO mp) >>= runReaderT (getMemPoolHolder holder)
