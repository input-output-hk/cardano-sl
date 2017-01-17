{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

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
       , UtxoIter
       , runUtxoIterator
       , runUtxoMapIterator
       , getFilteredUtxo

       -- * Sanity checks
       , sanityCheckUtxo
       ) where

import qualified Data.Map             as M
import qualified Data.Text.Buildable
import qualified Database.RocksDB     as Rocks
import           Formatting           (bprint, build, sformat, (%))
import           Serokell.Util.Text   (listJson, pairF)
import           System.Wlog          (WithLogger, logError)
import           Universum

import           Pos.Binary.Class     (encodeStrict)
import           Pos.Binary.Types     ()
import           Pos.DB.Class         (MonadDB, getUtxoDB)
import           Pos.DB.Error         (DBError (..))
import           Pos.DB.Functions     (RocksBatchOp (..), encodeWithKeyPrefix, rocksGetBi)
import           Pos.DB.GState.Common (getBi, putBi)
import           Pos.DB.Iterator      (DBIterator, DBMapIterator, IterType,
                                       MonadDBIterator (..), mapIterator, runIterator)
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

instance Buildable UtxoOp where
    build (DelTxIn txIn)           =
        bprint ("DelTxIn ("%build%")") txIn
    build (AddTxOut txIn txOutAux) =
        bprint ("AddTxOut ("%build%", "%listJson%")")
        txIn (map (bprint pairF) $ txOutStake txOutAux)

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

data UtxoIter

instance MonadDBIterator UtxoIter where
    type IterKey UtxoIter = TxIn
    type IterValue UtxoIter = TxOutAux
    iterKeyPrefix _ = "t/"

runUtxoMapIterator
    :: forall v m ssc a . (MonadDB ssc m, MonadMask m)
    => DBMapIterator UtxoIter v m a -> (IterType UtxoIter -> v) -> m a
runUtxoMapIterator iter f = mapIterator @UtxoIter @v iter f =<< getUtxoDB

runUtxoIterator
    :: forall m ssc a . (MonadDB ssc m, MonadMask m)
    => DBIterator UtxoIter m a -> m a
runUtxoIterator iter = runIterator @UtxoIter iter =<< getUtxoDB

filterUtxo
    :: forall ssc m . (MonadDB ssc m, MonadMask m)
    => (IterType UtxoIter -> Bool)
    -> m Utxo
filterUtxo p = runUtxoIterator (step mempty)
  where
    step res = nextItem @_ @(IterType UtxoIter) >>= maybe (pure res) (\e@(k, v) ->
      if | p e       -> step (M.insert (txInHash k, txInIndex k) v res)
         | otherwise -> step res)

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
        runUtxoMapIterator (step (mkCoin 0)) (map snd . txOutStake . snd)
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

genUtxoKey :: ByteString
genUtxoKey = "ut/gutxo"

txInKey :: TxIn -> ByteString
txInKey = encodeWithKeyPrefix @UtxoIter

----------------------------------------------------------------------------
-- Details
----------------------------------------------------------------------------

getGenUtxoMaybe :: MonadDB ssc m => m (Maybe Utxo)
getGenUtxoMaybe = getUtxoDB >>= rocksGetBi genUtxoKey >>= traverse (return . M.fromList)
