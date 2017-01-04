{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.DB.Utxo
       ( BatchOp (..)
       , getTip
       , getBot
       , getGenUtxo
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
import           Pos.DB.Class      (MonadDB, getUtxoDB)
import           Pos.DB.DBIterator (DBIterator, DBMapIterator, mapIterator, runIterator)
import           Pos.DB.Error      (DBError (..))
import           Pos.DB.Functions  (rocksDelete, rocksGetBi, rocksPutBi, rocksWriteBatch,
                                    traverseAllEntries)
import           Pos.DB.Types      (DB)
import           Pos.Types         (Address, Coin, HeaderHash, StakeholderId, TxIn (..),
                                    TxOutAux, Utxo, belongsTo, sumCoins, txOutStake)
import           Pos.Types.Coin    (unsafeIntegerToCoin)
import           Pos.Util          (maybeThrow)

data BatchOp ssc
    = DelTxIn TxIn
    | AddTxOut TxIn
               TxOutAux
    | PutTip (HeaderHash ssc)
    | PutFtsSum Coin
    | PutFtsStake StakeholderId Coin

-- | Get current TIP from Utxo DB.
getTip :: (MonadThrow m, MonadDB ssc m) => m (HeaderHash ssc)
getTip = maybeThrow (DBMalformed "no tip in Utxo DB") =<< getTipMaybe

-- | Get the hash of the first genesis block from Utxo DB
getBot :: (MonadThrow m, MonadDB ssc m) => m (HeaderHash ssc)
getBot = maybeThrow (DBMalformed "no bot in Utxo DB") =<< getBotMaybe

-- | Get genesis utxo
getGenUtxo :: (MonadThrow m, MonadDB ssc m) => m Utxo
getGenUtxo = maybeThrow (DBMalformed "no genesis utxo in Utxo DB") =<< getGenUtxoMaybe

-- | Get total amount of stake to be used for follow-the-satoshi. It's
-- different from total amount of coins in the system.
getTotalFtsStake :: (MonadThrow m, MonadDB ssc m) => m Coin
getTotalFtsStake = maybe (throwM $ DBMalformed "no 'ftssum' in Utxo DB") pure =<< getFtsSumMaybe

putTxOut :: MonadDB ssc m => TxIn -> TxOutAux -> m ()
putTxOut = putBi . txInKey

deleteTxOut :: MonadDB ssc m => TxIn -> m ()
deleteTxOut = delete . txInKey

getTxOut :: MonadDB ssc m => TxIn -> m (Maybe TxOutAux)
getTxOut = getBi . txInKey

getTxOutFromDB :: (MonadIO m, MonadThrow m) => TxIn -> DB ssc -> m (Maybe TxOutAux)
getTxOutFromDB txIn = rocksGetBi (txInKey txIn)

getFtsStake :: MonadDB ssc m => StakeholderId -> m (Maybe Coin)
getFtsStake pkHash = rocksGetBi (ftsStakeKey pkHash) =<< getUtxoDB

writeBatchToUtxo :: MonadDB ssc m => [BatchOp ssc] -> m ()
writeBatchToUtxo batch = rocksWriteBatch (map toRocksOp batch) =<< getUtxoDB

prepareUtxoDB
    :: forall ssc m.
       MonadDB ssc m
    => Utxo -> HeaderHash ssc -> m ()
prepareUtxoDB customUtxo initialTip = do
    putIfEmpty getTipMaybe putGenesisTip
    putIfEmpty getBotMaybe putGenesisBot
    putIfEmpty getGenUtxoMaybe putGenesisUtxo
    putIfEmpty getFtsSumMaybe putUtxo
    putIfEmpty getFtsSumMaybe putFtsStakes
    putIfEmpty getFtsSumMaybe putGenesisSum
  where
    totalCoins = sumCoins $ map snd $ concatMap txOutStake $ toList customUtxo
    putIfEmpty
        :: forall a.
           (m (Maybe a)) -> m () -> m ()
    putIfEmpty getter putter = maybe putter (const pass) =<< getter
    putGenesisTip = putTip initialTip
    putGenesisBot = putBot initialTip
    -- Will 'panic' if the result doesn't fit into Word64 (which should never
    -- happen)
    putGenesisSum = putTotalFtsStake (unsafeIntegerToCoin totalCoins)
    putGenesisUtxo = putGenUtxo customUtxo
    putUtxo = mapM_ putTxOut' $ M.toList customUtxo
    putTxOut' ((txid, id), txout) = putTxOut (TxIn txid id) txout
    putFtsStakes = mapM_ putFtsStake' $ M.toList customUtxo
    putFtsStake' (_, toaux) = mapM (uncurry putFtsStake) (txOutStake toaux)

putTip :: MonadDB ssc m => HeaderHash ssc -> m ()
putTip h = getUtxoDB >>= rocksPutBi tipKey h

putBot :: MonadDB ssc m => HeaderHash ssc -> m ()
putBot h = getUtxoDB >>= rocksPutBi botKey h

putGenUtxo :: MonadDB ssc m => Utxo -> m ()
putGenUtxo utxo = getUtxoDB >>= rocksPutBi genUtxoKey (M.toList utxo)

putTotalFtsStake :: MonadDB ssc m => Coin -> m ()
putTotalFtsStake c = getUtxoDB >>= rocksPutBi ftsSumKey c

----------------------------------------------------------------------------
-- Iteration
----------------------------------------------------------------------------

iterateByStake
    :: forall ssc m . (MonadDB ssc m, MonadMask m)
    => ((StakeholderId, Coin) -> m ())
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
toRocksOp (AddTxOut txIn txOut) = Rocks.Put (txInKey txIn) (encodeStrict txOut)
toRocksOp (DelTxIn txIn)        = Rocks.Del $ txInKey txIn
toRocksOp (PutTip h)            = Rocks.Put tipKey (encodeStrict h)
toRocksOp (PutFtsSum c)         = Rocks.Put ftsSumKey (encodeStrict c)
toRocksOp (PutFtsStake ad c)    = Rocks.Put (ftsStakeKey ad) (encodeStrict c)

tipKey :: ByteString
tipKey = "btip"

botKey :: ByteString
botKey = "bbot"

genUtxoKey :: ByteString
genUtxoKey = "gutxo"

txInKey :: TxIn -> ByteString
-- [CSL-379] Restore prefix after we have proper iterator
-- txInKey = (<> "t") . encodeStrict
txInKey = encodeStrict

ftsStakeKey :: StakeholderId -> ByteString
-- [CSL-379] Restore prefix after we have proper iterator
-- ftsStakeKey = (<> "s") . encodeStrict
ftsStakeKey = encodeStrict

ftsSumKey :: ByteString
ftsSumKey = "ftssum"

getTipMaybe :: (MonadDB ssc m) => m (Maybe (HeaderHash ssc))
getTipMaybe = getUtxoDB >>= rocksGetBi tipKey

getBotMaybe :: MonadDB ssc m => m (Maybe (HeaderHash ssc))
getBotMaybe = getUtxoDB >>= rocksGetBi botKey

getGenUtxoMaybe :: MonadDB ssc m => m (Maybe Utxo)
getGenUtxoMaybe = getUtxoDB >>= rocksGetBi genUtxoKey >>= traverse (return . M.fromList)

getFtsSumMaybe :: (MonadDB ssc m) => m (Maybe Coin)
getFtsSumMaybe = getUtxoDB >>= rocksGetBi ftsSumKey

putFtsStake :: MonadDB ssc m => StakeholderId -> Coin -> m ()
putFtsStake = putBi . ftsStakeKey
