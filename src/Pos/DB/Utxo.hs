{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.DB.Utxo
       ( BatchOp (..)
       , getTip
       , getTotalCoins
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

import qualified Data.Map          as M
import qualified Database.RocksDB  as Rocks
import           Universum

import           Pos.Binary.Class  (Bi, encodeStrict)
import           Pos.DB.Class      (MonadDB, getUtxoDB)
import           Pos.DB.DBIterator (DBIterator, DBMapIterator, mapIterator, withIterator)
import           Pos.DB.Error      (DBError (..))
import           Pos.DB.Functions  (rocksDelete, rocksGetBi, rocksPutBi, rocksWriteBatch,
                                    traverseAllEntries)
import           Pos.DB.Types      (DB)
import           Pos.Types         (Address, Coin, HeaderHash, TxIn (..), TxOut (..), Utxo,
                                    belongsTo, genesisHash)

data BatchOp ssc
    = DelTxIn TxIn
    | AddTxOut TxIn
               TxOut
    | PutTip (HeaderHash ssc)
    | PutTotal Coin

-- | Get current TIP from Utxo DB.
getTip :: (MonadThrow m, MonadDB ssc m) => m (HeaderHash ssc)
getTip = maybe (throwM $ DBMalformed "no tip in Utxo DB") pure =<< getTipMaybe

-- | Get current TIP from Utxo DB.
getTotalCoins :: (MonadThrow m, MonadDB ssc m) => m Coin
getTotalCoins = maybe (throwM $ DBMalformed "no 'sum' in Utxo DB") pure =<< getSumMaybe

putTxOut :: MonadDB ssc m => TxIn -> TxOut -> m ()
putTxOut = putBi . utxoKey

deleteTxOut :: MonadDB ssc m => TxIn -> m ()
deleteTxOut = delete . utxoKey

getTxOut :: MonadDB ssc m => TxIn -> m (Maybe TxOut)
getTxOut = getBi . utxoKey

getTxOutFromDB :: (MonadIO m, MonadThrow m) => TxIn -> DB ssc -> m (Maybe TxOut)
getTxOutFromDB txIn = rocksGetBi (utxoKey txIn)

writeBatchToUtxo :: MonadDB ssc m => [BatchOp ssc] -> m ()
writeBatchToUtxo batch = rocksWriteBatch (map toRocksOp batch) =<< getUtxoDB

prepareUtxoDB :: forall ssc m . MonadDB ssc m => Utxo -> m ()
prepareUtxoDB customUtxo = do
    putIfEmpty getTipMaybe putGenesisTip
    putIfEmpty getSumMaybe putUtxo
    putIfEmpty getSumMaybe putGenesisSum
  where
    totalCoins = sum $ map txOutValue $ toList customUtxo
    putIfEmpty
        :: forall a.
           (m (Maybe a)) -> m () -> m ()
    putIfEmpty getter putter = maybe putter (const pass) =<< getter
    putGenesisTip = putTip genesisHash
    putGenesisSum = putTotalCoins totalCoins
    putUtxo = mapM_ putTxOut' $ M.toList customUtxo
    putTxOut' ((txid, id), txout) = putTxOut (TxIn txid id) txout

putTip :: MonadDB ssc m => HeaderHash ssc -> m ()
putTip h = getUtxoDB >>= rocksPutBi tipKey h

putTotalCoins :: MonadDB ssc m => Coin -> m ()
putTotalCoins c = getUtxoDB >>= rocksPutBi sumKey c

iterateByUtxo
    :: forall ssc m . (MonadDB ssc m, MonadMask m)
    => ((TxIn, TxOut) -> m ())
    -> m ()
iterateByUtxo callback = do
    db <- getUtxoDB
    traverseAllEntries db (pure ()) $ const $ curry callback

filterUtxo
    :: forall ssc m . (MonadDB ssc m, MonadMask m)
    => ((TxIn, TxOut) -> Bool)
    -> m Utxo
filterUtxo p = do
    db <- getUtxoDB
    traverseAllEntries db (pure M.empty) $ \m k v ->
        if p (k, v)
        then return $ M.insert (txInHash k, txInIndex k) v m
        else return m

withUtxoIterator :: (MonadDB ssc m, MonadMask m)
                 => DBIterator m a -> m a
withUtxoIterator iter = withIterator iter =<< getUtxoDB

mapUtxoIterator :: forall u v m ssc a . (MonadDB ssc m, MonadMask m)
                => DBMapIterator (u -> v) m a -> (u -> v) -> m a
mapUtxoIterator iter f = mapIterator @u @v iter f =<< getUtxoDB

-- | Get small sub-utxo containing only outputs of given address
getFilteredUtxo :: (MonadDB ssc m, MonadMask m) => Address -> m Utxo
getFilteredUtxo addr = filterUtxo $ \(_, out) -> out `belongsTo` addr

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
toRocksOp (PutTotal c)          = Rocks.Put sumKey (encodeStrict c)

tipKey :: ByteString
tipKey = "btip"

utxoKey :: TxIn -> ByteString
utxoKey = (<>) "t" . encodeStrict

sumKey :: ByteString
sumKey = "sum"

getTipMaybe :: (MonadDB ssc m) => m (Maybe (HeaderHash ssc))
getTipMaybe = getUtxoDB >>= rocksGetBi tipKey

getSumMaybe :: (MonadDB ssc m) => m (Maybe Coin)
getSumMaybe = getUtxoDB >>= rocksGetBi sumKey
