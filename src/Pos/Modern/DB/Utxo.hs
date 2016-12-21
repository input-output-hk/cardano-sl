{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Modern.DB.Utxo
       ( BatchOp (..)
       , getTip
       , putTxOut
       , deleteTxOut
       , getTxOut
       , writeBatchToUtxo
       , getTxOutFromDB
       , prepareUtxoDB
       , iterateByUtxo
       , withUtxoIterator
       , mapUtxoIterator
       , getFilteredUtxo
       ) where

import qualified Database.RocksDB         as Rocks
import           Universum

import           Pos.Binary.Class         (Bi, encodeStrict)
import           Pos.Modern.DB.Class      (MonadDB, getUtxoDB)
import           Pos.Modern.DB.DBIterator (DBIterator, DBMapIterator, mapIterator,
                                           withIterator)
import           Pos.Modern.DB.Error      (DBError (..))
import           Pos.Modern.DB.Functions  (iterateByAllEntries, rocksDelete, rocksGetBi,
                                           rocksPutBi, rocksWriteBatch)
import           Pos.Modern.DB.Types      (DB)
import           Pos.Types                (Address, HeaderHash, TxIn (..), TxOut, Utxo,
                                           genesisHash)

data BatchOp ssc = DelTxIn TxIn | AddTxOut TxIn TxOut | PutTip (HeaderHash ssc)

-- | Get current TIP from Utxo DB.
getTip :: (MonadThrow m, MonadDB ssc m) => m (HeaderHash ssc)
getTip = maybe (throwM $ DBMalformed "no tip in Utxo DB") pure =<< getTipMaybe

putTxOut :: MonadDB ssc m => TxIn -> TxOut -> m ()
putTxOut = putBi . utxoKey

deleteTxOut :: MonadDB ssc m => TxIn -> m ()
deleteTxOut = delete . utxoKey

getTxOut :: MonadDB ssc m => TxIn -> m (Maybe TxOut)
getTxOut = getBi . utxoKey

getTxOutFromDB :: MonadIO m => TxIn -> DB ssc -> m (Maybe TxOut)
getTxOutFromDB txIn = rocksGetBi (utxoKey txIn)

writeBatchToUtxo :: MonadDB ssc m => [BatchOp ssc] -> m ()
writeBatchToUtxo batch = rocksWriteBatch (map toRocksOp batch) =<< getUtxoDB

prepareUtxoDB :: MonadDB ssc m => m ()
prepareUtxoDB = maybe putGenesisTip (const pass) =<< getTipMaybe
  where
    putGenesisTip = putTip genesisHash

-- | Put new TIP to Utxo DB.
putTip :: MonadDB ssc m => HeaderHash ssc -> m ()
putTip h = getUtxoDB >>= rocksPutBi tipKey h

iterateByUtxo :: forall ssc m . (MonadDB ssc m, MonadMask m) => ((TxIn, TxOut) -> m ()) -> m ()
iterateByUtxo callback = getUtxoDB >>= flip iterateByAllEntries callback

withUtxoIterator :: (MonadDB ssc m, MonadMask m)
                 => DBIterator m a -> m a
withUtxoIterator iter = withIterator iter =<< getUtxoDB

mapUtxoIterator :: forall u v m ssc a . (MonadDB ssc m, MonadMask m)
                 => DBMapIterator (u->v) m a -> (u->v) -> m a
mapUtxoIterator iter f = mapIterator @u @v iter f =<< getUtxoDB

-- | Get small sub-utxo containing only outputs of given address
getFilteredUtxo :: MonadDB ssc m => Address -> m Utxo
getFilteredUtxo = undefined

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

getBi
    :: (MonadDB ssc m, Bi v)
    => ByteString -> m (Maybe v)
getBi k = rocksGetBi k =<< getUtxoDB

putBi
    :: (MonadDB ssc m, Bi v)
    => ByteString -> v -> m ()
putBi k v = rocksPutBi k v =<< getUtxoDB

delete :: (MonadDB ssc m) => ByteString -> m ()
delete k = rocksDelete k =<< getUtxoDB

toRocksOp :: BatchOp ssc -> Rocks.BatchOp
toRocksOp (AddTxOut txIn txOut) = Rocks.Put (utxoKey txIn) (encodeStrict txOut)
toRocksOp (DelTxIn txIn)        = Rocks.Del $ utxoKey txIn
toRocksOp (PutTip h)            = Rocks.Put tipKey (encodeStrict h)

tipKey :: ByteString
tipKey = "tip"

utxoKey :: TxIn -> ByteString
utxoKey = (<>) "t" . encodeStrict

getTipMaybe :: (MonadDB ssc m) => m (Maybe (HeaderHash ssc))
getTipMaybe = getUtxoDB >>= rocksGetBi tipKey
