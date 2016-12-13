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
       , runUVHolder
       ) where
import qualified Control.Concurrent.STM          as STM
import           Control.Monad.Reader            (ReaderT (ReaderT))
import           Universum

import           Pos.Modern.Txp.Storage.UtxoView (UtxoView)
import qualified Pos.Modern.Txp.Storage.UtxoView as UV
import           Pos.Types                       (TxIn (..), TxOut)

class Monad m => MonadUtxoRead ssc m | m -> ssc where
    getTxOut :: TxIn -> m (Maybe TxOut)

class MonadUtxoRead ssc m => MonadUtxo ssc m | m -> ssc where
    putTxOut :: TxIn -> TxOut -> m ()
    delTxIn :: TxIn -> m ()

data MemPool ssc

class Monad m => TxpLDMonad ssc m where
    getUtxoView :: m (UtxoView ssc)
    getMemPool  :: m (MemPool ssc)

newtype UtxoViewHolder ssc m a = UtxoViewHolder
    { getUtxoViewHolder :: ReaderT (STM.TVar (UtxoView ssc)) m a
    } deriving (Functor, Applicative, Monad, MonadIO)

instance MonadIO m => MonadUtxoRead ssc (UtxoViewHolder ssc m) where
    getTxOut key = UtxoViewHolder ask >>= (atomically . STM.readTVar >=> UV.getTxOut key)

instance (MonadIO m, MonadUtxoRead ssc (UtxoViewHolder ssc m))
       => MonadUtxo ssc (UtxoViewHolder ssc m) where
    putTxOut key val = UtxoViewHolder ask >>= atomically . flip STM.modifyTVar' (UV.putTxOut key val)
    delTxIn key = UtxoViewHolder ask >>= atomically . flip STM.modifyTVar' (UV.delTxIn key)

runUVHolder :: MonadIO m => UtxoView ssc -> UtxoViewHolder ssc m a -> m a
runUVHolder mp holder = liftIO (STM.newTVarIO mp) >>= runReaderT (getUtxoViewHolder holder)
