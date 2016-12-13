module Pos.Modern.DB.Utxo
       ( getTip
       , putTip
       , putTxOut
       , deleteTxOut
       , getTxOut
       , writeBatchToUtxo
       , BatchOp (..)
       ) where

import qualified Database.RocksDB        as Rocks
import           Universum

import           Pos.Binary              (Bi, encode)
import           Pos.Binary              (encodeStrict)
import           Pos.Modern.DB           (MonadDB, getUtxoDB)
import           Pos.Modern.DB.Functions (rocksDelete, rocksGetBi, rocksPutBi,
                                          rocksWriteBatch)
import           Pos.Types               (HeaderHash, TxIn (..), TxOut)

data BatchOp = DelTxIn TxIn | AddTxOut TxIn TxOut

-- | Get current TIP from Utxo DB.
getTip :: MonadDB ssc m => m (Maybe (HeaderHash ssc))
getTip = getUtxoDB >>= rocksGetBi "tip"

-- | Put new TIP to Utxo DB.
putTip :: MonadDB ssc m => HeaderHash ssc -> m ()
putTip h = getUtxoDB >>= rocksPutBi "tip" h

putTxOut :: MonadDB ssc m => TxIn -> TxOut -> m ()
putTxOut = putBi . utxoKey

deleteTxOut :: MonadDB ssc m => TxIn -> m ()
deleteTxOut = delete . utxoKey

getTxOut :: MonadDB ssc m => TxIn -> m (Maybe TxOut)
getTxOut = getBi . utxoKey

writeBatchToUtxo :: MonadDB ssc m => [BatchOp] -> m ()
writeBatchToUtxo batch = rocksWriteBatch (map toRocksOp batch) =<< getUtxoDB

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

toRocksOp :: BatchOp -> Rocks.BatchOp
toRocksOp (AddTxOut txIn txOut) = Rocks.Put (utxoKey txIn) (encodeStrict txOut)
toRocksOp (DelTxIn txIn)        = Rocks.Del $ utxoKey txIn

utxoKey :: TxIn -> ByteString
utxoKey = (<>) "t" . encodeStrict
