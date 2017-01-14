{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Part of GState DB which stores unspent transaction outputs.

module Pos.DB.GState.Utxo
       (
         -- * Getters
         getGenUtxo
       , getTxOutFromDB
       , getTxOut

       -- * Operations
       , UtxoOp (..)

       -- * Initialization
       , prepareGStateUtxo

       -- * Iteration
       , iterateByTx
       , runUtxoIterator
       , mapUtxoIterator
       , getFilteredUtxo

       -- * Sanity checks
       , sanityCheckUtxo
       ) where

import qualified Data.Map             as M
import qualified Database.RocksDB     as Rocks
import           Formatting           (sformat, (%))
import           System.Wlog          (WithLogger, logError)
import           Universum

import           Pos.Binary.Class     (encodeStrict)
import           Pos.Binary.Types     ()
import           Pos.DB.Class         (MonadDB, getUtxoDB)
import           Pos.DB.DBIterator    (DBIterator, DBMapIterator, mapIterator,
                                       runIterator)
import           Pos.DB.Error         (DBError (..))
import           Pos.DB.Functions     (RocksBatchOp (..), WithKeyPrefix (..),
                                       encodeWithKeyPrefix, rocksGetBi,
                                       traverseAllEntries)
import           Pos.DB.GState.Common (getBi, putBi)
import           Pos.DB.Types         (DB)
import           Pos.Types            (Address, Coin, TxIn (..), TxOutAux, Utxo,
                                       belongsTo, coinF, mkCoin, sumCoins, txOutStake,
                                       unsafeAddCoin, unsafeIntegerToCoin)
import           Pos.Util             (Color (..), colorize, maybeThrow)
import           Pos.Util.Iterator    (nextItem)

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

-- | Get genesis utxo
getGenUtxo :: (MonadThrow m, MonadDB ssc m) => m Utxo
getGenUtxo = maybeThrow (DBMalformed "no genesis utxo in Utxo DB") =<< getGenUtxoMaybe

getTxOut :: MonadDB ssc m => TxIn -> m (Maybe TxOutAux)
getTxOut = getBi . txInKey

getTxOutFromDB :: (MonadIO m, MonadThrow m) => TxIn -> DB ssc -> m (Maybe TxOutAux)
getTxOutFromDB txIn = rocksGetBi (txInKey txIn)

----------------------------------------------------------------------------
-- Batch operations
----------------------------------------------------------------------------

data UtxoOp
    = DelTxIn !TxIn
    | AddTxOut !TxIn
               !TxOutAux

instance RocksBatchOp UtxoOp where
    toBatchOp (AddTxOut txIn txOut) =
        [Rocks.Put (txInKey txIn) (encodeStrict txOut)]
    toBatchOp (DelTxIn txIn) = [Rocks.Del $ txInKey txIn]

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareGStateUtxo
    :: forall ssc m.
       MonadDB ssc m
    => Utxo -> m ()
prepareGStateUtxo genesisUtxo =
    putIfEmpty getGenUtxoMaybe putGenesisUtxo
  where
    putIfEmpty
        :: forall a.
           (m (Maybe a)) -> m () -> m ()
    putIfEmpty getter putter = maybe putter (const pass) =<< getter
    putGenesisUtxo = do
        let utxoList = M.toList genesisUtxo
        mapM_ putTxOut' utxoList
        putBi genUtxoKey utxoList
    putTxOut' ((txid, id), txout) = putTxOut (TxIn txid id) txout

putTxOut :: MonadDB ssc m => TxIn -> TxOutAux -> m ()
putTxOut = putBi . txInKey

----------------------------------------------------------------------------
-- Iteration
----------------------------------------------------------------------------

type IterType = (TxIn, TxOutAux)

iterateByTx :: forall v m ssc a . (MonadDB ssc m, MonadMask m)
                => DBMapIterator IterType v m a -> (IterType -> v) -> m a
iterateByTx iter f = mapIterator @IterType @v iter f =<< getUtxoDB

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

runUtxoIterator
    :: (MonadDB ssc m, MonadMask m)
    => DBIterator v m a -> m a
runUtxoIterator iter = runIterator iter =<< getUtxoDB

mapUtxoIterator :: forall u v m ssc a . (MonadDB ssc m, MonadMask m)
                => DBMapIterator u v m a -> (u -> v) -> m a
mapUtxoIterator iter f = mapIterator @u @v iter f =<< getUtxoDB

-- | Get small sub-utxo containing only outputs of given address
getFilteredUtxo :: (MonadDB ssc m, MonadMask m) => Address -> m Utxo
getFilteredUtxo addr = filterUtxo $ \(_, out) -> out `belongsTo` addr

----------------------------------------------------------------------------
-- Sanity checks
----------------------------------------------------------------------------

sanityCheckUtxo
    :: (MonadMask m, MonadDB ssc m, WithLogger m)
    => Coin -> m ()
sanityCheckUtxo expectedTotalStake = do
    calculatedTotalStake <-
        iterateByTx (step (mkCoin 0)) (map snd . txOutStake . snd)
    let fmt =
            ("Sum of stakes in Utxo differs from expected total stake (the former is "
             %coinF%", while the latter is "%coinF%")")
    let msg = sformat fmt calculatedTotalStake expectedTotalStake
    unless (calculatedTotalStake == expectedTotalStake) $ do
        logError $ colorize Red msg
        throwM $ DBMalformed msg
  where
    step sm = do
        n <- nextItem
        maybe
            (pure sm)
            (\stakes ->
                 step (sm `unsafeAddCoin` unsafeIntegerToCoin (sumCoins stakes)))
            n

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

instance WithKeyPrefix TxIn where
    keyPrefix _ = "t/"

genUtxoKey :: ByteString
genUtxoKey = "ut/gutxo"

txInKey :: TxIn -> ByteString
txInKey = encodeWithKeyPrefix

----------------------------------------------------------------------------
-- Details
----------------------------------------------------------------------------

getGenUtxoMaybe :: MonadDB ssc m => m (Maybe Utxo)
getGenUtxoMaybe = getUtxoDB >>= rocksGetBi genUtxoKey >>= traverse (return . M.fromList)
