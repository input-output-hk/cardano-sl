{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.DB.Utxo
       ( BatchOp (..)
       , getTip
       , getTotalFtsStake
       , putTxOut
       , deleteTxOut
       , getTxOutFromDB
       , getTxOut
       , getFtsStake
       , writeBatchToUtxo
       , prepareUtxoDB
       , iterateByUtxo
       , runUtxoIterator
       , mapUtxoIterator
       , getFilteredUtxo
       , iterateByStake
       ) where

import qualified Data.Map          as M
import qualified Database.RocksDB  as Rocks
import           Universum

import           Pos.Binary.Class  (Bi, encodeStrict)
import           Pos.Crypto        (PublicKey)
import           Pos.DB.Class      (MonadDB, getUtxoDB)
import           Pos.DB.DBIterator (DBIterator, DBMapIterator, mapIterator, runIterator)
import           Pos.DB.Error      (DBError (..))
import           Pos.DB.Functions  (rocksDelete, rocksGetBi, rocksPutBi, rocksWriteBatch,
                                    traverseAllEntries)
import           Pos.DB.Types      (DB)
import           Pos.Types         (Address, AddressHash, Coin, HeaderHash, TxIn (..),
                                    TxOutAux, Utxo, belongsTo, txOutStake)

data BatchOp ssc
    = DelTxIn TxIn
    | AddTxOut TxIn
               TxOutAux
    | PutTip (HeaderHash ssc)
    | PutFtsSum Coin
    | PutFtsStake (AddressHash PublicKey) Coin

-- | Get current TIP from Utxo DB.
getTip :: (MonadThrow m, MonadDB ssc m) => m (HeaderHash ssc)
getTip = maybe (throwM $ DBMalformed "no tip in Utxo DB") pure =<< getTipMaybe

-- | Get total amount of stake to be used for follow-the-satoshi. It's
-- different from total amount of coins in the system.
getTotalFtsStake :: (MonadThrow m, MonadDB ssc m) => m Coin
getTotalFtsStake = maybe (throwM $ DBMalformed "no 'ftssum' in Utxo DB") pure =<< getFtsSumMaybe

putTxOut :: MonadDB ssc m => TxIn -> TxOutAux -> m ()
putTxOut = putBi . tinKey

deleteTxOut :: MonadDB ssc m => TxIn -> m ()
deleteTxOut = delete . tinKey

getTxOut :: MonadDB ssc m => TxIn -> m (Maybe TxOutAux)
getTxOut = getBi . tinKey

getTxOutFromDB :: (MonadIO m, MonadThrow m) => TxIn -> DB ssc -> m (Maybe TxOutAux)
getTxOutFromDB txIn = rocksGetBi (tinKey txIn)

getFtsStake :: MonadDB ssc m => AddressHash PublicKey -> m (Maybe Coin)
getFtsStake pkHash = rocksGetBi (ftsStakeKey pkHash) =<< getUtxoDB

writeBatchToUtxo :: MonadDB ssc m => [BatchOp ssc] -> m ()
writeBatchToUtxo batch = rocksWriteBatch (map toRocksOp batch) =<< getUtxoDB

prepareUtxoDB
    :: forall ssc m.
       MonadDB ssc m
    => Utxo -> HeaderHash ssc -> m ()
prepareUtxoDB customUtxo initialTip = do
    putIfEmpty getTipMaybe putGenesisTip
    putIfEmpty getFtsSumMaybe putUtxo
    putIfEmpty getFtsSumMaybe putFtsStakes
    putIfEmpty getFtsSumMaybe putGenesisSum
  where
    totalCoins = sum $ map snd $ concatMap txOutStake $ toList customUtxo
    putIfEmpty
        :: forall a.
           (m (Maybe a)) -> m () -> m ()
    putIfEmpty getter putter = maybe putter (const pass) =<< getter
    putGenesisTip = putTip initialTip
    putGenesisSum = putTotalFtsStake totalCoins
    putUtxo = mapM_ putTxOut' $ M.toList customUtxo
    putTxOut' ((txid, id), txout) = putTxOut (TxIn txid id) txout
    putFtsStakes = mapM_ putFtsStake' $ M.toList customUtxo
    putFtsStake' (_, toaux) = mapM (uncurry putFtsStake) (txOutStake toaux)

putTip :: MonadDB ssc m => HeaderHash ssc -> m ()
putTip h = getUtxoDB >>= rocksPutBi tipKey h

putTotalFtsStake :: MonadDB ssc m => Coin -> m ()
putTotalFtsStake c = getUtxoDB >>= rocksPutBi ftsSumKey c

----------------------------------------------------------------------------
-- Iteration
----------------------------------------------------------------------------

iterateByStake
    :: forall ssc m . (MonadDB ssc m, MonadMask m)
    => ((AddressHash PublicKey, Coin) -> m ())
    -> m ()
iterateByStake callback = do
    db <- getUtxoDB
    traverseAllEntries db (pure ()) $ const $ curry callback

iterateByUtxo
    :: forall ssc m . (MonadDB ssc m, MonadMask m)
    => ((TxIn, TxOutAux) -> m ())
    -> m ()
iterateByUtxo callback = do
    db <- getUtxoDB
    traverseAllEntries db (pure ()) $ const $ curry callback

filterUtxo
    :: forall ssc m . (MonadDB ssc m, MonadMask m)
    => ((TxIn, TxOutAux) -> Bool)
    -> m Utxo
filterUtxo p = do
    db <- getUtxoDB
    traverseAllEntries db (pure M.empty) $ \m k v ->
        if p (k, v)
        then return $ M.insert (txInHash k, txInIndex k) v m
        else return m

runUtxoIterator :: (MonadDB ssc m, MonadMask m)
                 => DBIterator m a -> m a
runUtxoIterator iter = runIterator iter =<< getUtxoDB

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
toRocksOp (AddTxOut txIn txOut) = Rocks.Put (tinKey txIn) (encodeStrict txOut)
toRocksOp (DelTxIn txIn)        = Rocks.Del $ tinKey txIn
toRocksOp (PutTip h)            = Rocks.Put tipKey (encodeStrict h)
toRocksOp (PutFtsSum c)         = Rocks.Put ftsSumKey (encodeStrict c)
toRocksOp (PutFtsStake ad c)    = Rocks.Put (ftsStakeKey ad) (encodeStrict c)

tipKey :: ByteString
tipKey = "btip"

tinKey :: TxIn -> ByteString
-- [CSL-379] Restore prefix after we have proper iterator
-- tinKey = (<> "t") . encodeStrict
tinKey = encodeStrict

ftsStakeKey :: AddressHash PublicKey -> ByteString
-- [CSL-379] Restore prefix after we have proper iterator
-- ftsStakeKey = (<> "s") . encodeStrict
ftsStakeKey = encodeStrict

ftsSumKey :: ByteString
ftsSumKey = "ftssum"

getTipMaybe :: (MonadDB ssc m) => m (Maybe (HeaderHash ssc))
getTipMaybe = getUtxoDB >>= rocksGetBi tipKey

getFtsSumMaybe :: (MonadDB ssc m) => m (Maybe Coin)
getFtsSumMaybe = getUtxoDB >>= rocksGetBi ftsSumKey

putFtsStake :: MonadDB ssc m => AddressHash PublicKey -> Coin -> m ()
putFtsStake = putBi . ftsStakeKey
