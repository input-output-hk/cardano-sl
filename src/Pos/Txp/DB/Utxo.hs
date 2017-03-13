{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Part of GState DB which stores unspent transaction outputs.

module Pos.Txp.DB.Utxo
       (
         -- * Getters
         getTxOutFromDB
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
       , getFilteredGenUtxo

       -- * Sanity checks
       , sanityCheckUtxo
       ) where

import qualified Data.Map             as M
import qualified Data.Text.Buildable
import qualified Database.RocksDB     as Rocks
import           Formatting           (bprint, build, sformat, (%))
import           Serokell.Util        (Color (Red), colorize)
import           Serokell.Util.Text   (listJson, pairF)
import           System.Wlog          (WithLogger, logError)
import           Universum

import           Pos.Binary.Class     (encodeStrict)
import           Pos.Binary.Core      ()
import           Pos.DB.Class         (MonadDB, getUtxoDB)
import           Pos.DB.Error         (DBError (..))
import           Pos.DB.Functions     (RocksBatchOp (..), encodeWithKeyPrefix, rocksGetBi,
                                       rocksGetBytes)
import           Pos.DB.GState.Common (gsGetBi, gsPutBi, writeBatchGState)
import           Pos.DB.Iterator      (DBIteratorClass (..), DBnIterator, DBnMapIterator,
                                       IterType, runDBnIterator, runDBnMapIterator)
import           Pos.DB.Types         (DB, NodeDBs (_gStateDB))
import           Pos.Txp.Core         (TxIn (..), TxOutAux, addrBelongsTo, txOutStake)
import           Pos.Txp.Toil.Types   (Utxo)
import           Pos.Types            (Address, Coin, coinF, mkCoin, sumCoins,
                                       unsafeAddCoin, unsafeIntegerToCoin)
import           Pos.Util.Iterator    (nextItem)

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

getTxOut :: MonadDB m => TxIn -> m (Maybe TxOutAux)
getTxOut = gsGetBi . txInKey

getTxOutFromDB :: (MonadIO m, MonadThrow m) => TxIn -> DB -> m (Maybe TxOutAux)
getTxOutFromDB txIn = rocksGetBi (txInKey txIn)

----------------------------------------------------------------------------
-- Batch operations
----------------------------------------------------------------------------

data UtxoOp
    = DelTxIn !TxIn
    | AddTxOut !TxIn !TxOutAux
    | AddGenTxOut !TxIn !TxOutAux

instance Buildable UtxoOp where
    build (DelTxIn txIn)           =
        bprint ("DelTxIn ("%build%")") txIn
    build (AddTxOut txIn txOutAux) =
        bprint ("AddTxOut ("%build%", "%listJson%")")
        txIn (map (bprint pairF) $ txOutStake txOutAux)
    build (AddGenTxOut txIn txOutAux) =
        bprint ("AddGenTxOut ("%build%", "%listJson%")")
        txIn (map (bprint pairF) $ txOutStake txOutAux)

instance RocksBatchOp UtxoOp where
    toBatchOp (AddTxOut txIn txOut) =
        [Rocks.Put (txInKey txIn) (encodeStrict txOut)]
    toBatchOp (AddGenTxOut txIn txOut) =
        [Rocks.Put (genTxInKey txIn) (encodeStrict txOut)]
    toBatchOp (DelTxIn txIn) = [Rocks.Del $ txInKey txIn]

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareGStateUtxo
    :: forall m.
       MonadDB m
    => Utxo -> m ()
prepareGStateUtxo genesisUtxo =
    putIfEmpty genUtxoExists putGenesisUtxo
  where
    putIfEmpty :: m Bool -> m () -> m ()
    putIfEmpty exists putter = whenM (not <$> exists) $ putter
    putGenesisUtxo = do
        let utxoList = M.toList genesisUtxo
        writeBatchGState $ concat $ map createBatchOp utxoList
        gsPutBi genUtxoFlagKey True
    createBatchOp (txin, txout) =
        [AddTxOut txin txout , AddGenTxOut txin txout]

----------------------------------------------------------------------------
-- Iteration
----------------------------------------------------------------------------

data UtxoIter
data GenUtxoIter

instance DBIteratorClass UtxoIter where
    type IterKey UtxoIter = TxIn
    type IterValue UtxoIter = TxOutAux
    iterKeyPrefix _ = iterationUtxoPrefix

instance DBIteratorClass GenUtxoIter where
    type IterKey GenUtxoIter = TxIn
    type IterValue GenUtxoIter = TxOutAux
    iterKeyPrefix _ = iterationGenUtxoPrefix

runUtxoIterator
    :: forall i m a .
       ( MonadDB m
       , DBIteratorClass i
       , IterKey i ~ TxIn
       , IterValue i ~ TxOutAux
       )
    => DBnIterator i a
    -> m a
runUtxoIterator = runDBnIterator @i _gStateDB

runUtxoMapIterator
    :: forall i v m a .
       ( MonadDB m
       , DBIteratorClass i
       , IterKey i ~ TxIn
       , IterValue i ~ TxOutAux
       )
    => DBnMapIterator i v a
    -> (IterType i -> v)
    -> m a
runUtxoMapIterator = runDBnMapIterator @i _gStateDB

filterUtxo
    :: forall i m .
       ( MonadDB m
       , DBIteratorClass i
       , IterKey i ~ TxIn
       , IterValue i ~ TxOutAux
       )
    => (IterType i -> Bool)
    -> m Utxo
filterUtxo p = runUtxoIterator @i (step mempty)
  where
    step res = nextItem >>= maybe (pure res) (\e@(k, v) ->
      if | p e       -> step (M.insert k v res)
         | otherwise -> step res)

-- | Get small sub-utxo containing only outputs of given address
getFilteredUtxo'
    :: forall i m .
       ( MonadDB m
       , DBIteratorClass i
       , IterKey i ~ TxIn
       , IterValue i ~ TxOutAux
       )
    => Address -> m Utxo
getFilteredUtxo' addr = filterUtxo @i $ \(_, out) -> out `addrBelongsTo` addr

getFilteredUtxo :: MonadDB m => Address -> m Utxo
getFilteredUtxo = getFilteredUtxo' @UtxoIter

getFilteredGenUtxo :: MonadDB m => Address -> m Utxo
getFilteredGenUtxo = getFilteredUtxo' @GenUtxoIter

----------------------------------------------------------------------------
-- Sanity checks
----------------------------------------------------------------------------

sanityCheckUtxo
    :: (MonadDB m, WithLogger m)
    => Coin -> m ()
sanityCheckUtxo expectedTotalStake = do
    calculatedTotalStake <-
        runUtxoMapIterator @UtxoIter (step (mkCoin 0)) (map snd . txOutStake . snd)
    let fmt =
            ("Sum of stakes in Utxo differs from expected total stake (the former is "
             %coinF%", while the latter is "%coinF%")")
    let msg = sformat fmt calculatedTotalStake expectedTotalStake
    unless (calculatedTotalStake == expectedTotalStake) $ do
        logError $ colorize Red msg
        throwM $ DBMalformed msg
  where
    step sm =
        nextItem >>= \case
            Nothing -> pure sm
            Just stakes ->
                step
                    (sm `unsafeAddCoin`
                     unsafeIntegerToCoin (sumCoins @[Coin] stakes))

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

txInKey :: TxIn -> ByteString
txInKey = encodeWithKeyPrefix @UtxoIter

iterationUtxoPrefix :: ByteString
iterationUtxoPrefix = "ut/t/"

genTxInKey :: TxIn -> ByteString
genTxInKey = encodeWithKeyPrefix @GenUtxoIter

iterationGenUtxoPrefix :: ByteString
iterationGenUtxoPrefix = "ut/gt/"

genUtxoFlagKey :: ByteString
genUtxoFlagKey = "ut/gutxo"

----------------------------------------------------------------------------
-- Details
----------------------------------------------------------------------------

genUtxoExists :: MonadDB m => m Bool
genUtxoExists = isJust <$> (getUtxoDB >>= rocksGetBytes genUtxoFlagKey)
