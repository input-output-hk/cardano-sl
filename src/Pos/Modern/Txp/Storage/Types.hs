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
       , MonadTxpLD (..)
       , runTxpLDHolder
       ) where
import qualified Control.Concurrent.STM          as STM
import           Control.Monad.Reader            (ReaderT (ReaderT))
import qualified Data.HashSet                    as HS (empty)
import           Universum

import           Pos.Modern.Txp.Storage.UtxoView (UtxoView)
import qualified Pos.Modern.Txp.Storage.UtxoView as UV
import           Pos.Types                       (Tx, TxIn (..), TxOut)

class Monad m => MonadUtxoRead ssc m | m -> ssc where
    getTxOut :: TxIn -> m (Maybe TxOut)

class MonadUtxoRead ssc m => MonadUtxo ssc m | m -> ssc where
    putTxOut :: TxIn -> TxOut -> m ()
    delTxIn :: TxIn -> m ()

type MemPool = HashSet Tx

class Monad m => MonadTxpLD ssc m | m -> ssc where
    getUtxoView :: m (UtxoView ssc)
    getMemPool  :: m MemPool
    setUtxoView :: UtxoView ssc -> m ()
    setMemPool  :: MemPool -> m ()

----------------------------------------------------------------------------
-- Holders
----------------------------------------------------------------------------
data TxpLDWrap ssc = TxpLDWrap
    {
      utxoView :: !(STM.TVar (UtxoView ssc))
    , memPool  :: !(STM.TVar MemPool)
    }

newtype TxpLDHolder ssc m a = TxpLDHolder
    { getTxpLDHolder :: ReaderT (TxpLDWrap ssc) m a
    } deriving (Functor, Applicative, Monad, MonadIO)

instance MonadIO m => MonadUtxoRead ssc (TxpLDHolder ssc m) where
    getTxOut key = TxpLDHolder (asks utxoView) >>=
                   (atomically . STM.readTVar >=> UV.getTxOut key)

instance (MonadIO m, MonadUtxoRead ssc (TxpLDHolder ssc m))
       => MonadUtxo ssc (TxpLDHolder ssc m) where
    putTxOut key val = TxpLDHolder (asks utxoView) >>=
                       atomically . flip STM.modifyTVar' (UV.putTxOut key val)
    delTxIn key = TxpLDHolder (asks utxoView) >>=
                  atomically . flip STM.modifyTVar' (UV.delTxIn key)

runTxpLDHolder :: MonadIO m => TxpLDHolder ssc m a -> UtxoView ssc -> m a
runTxpLDHolder holder mp = TxpLDWrap
                       <$> liftIO (STM.newTVarIO mp)
                       <*> liftIO (STM.newTVarIO HS.empty)
                       >>= runReaderT (getTxpLDHolder holder)
