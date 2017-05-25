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

       -- * Get utxo
       , getAllPotentiallyHugeUtxo

       -- * Sanity checks
       , sanityCheckUtxo
       ) where

import           Universum

import qualified Data.HashSet         as HS
import qualified Data.Map             as M
import qualified Data.Text.Buildable
import qualified Database.RocksDB     as Rocks
import           Formatting           (bprint, build, sformat, (%))
import           Serokell.Util        (Color (Red), colorize)
import           Serokell.Util.Text   (listJson, pairF)
import           System.Wlog          (WithLogger, logError)

import           Pos.Binary.Class     (encodeStrict)
import           Pos.Binary.Core      ()
import           Pos.Core.Address     (AddressIgnoringAttributes (..))
import           Pos.DB.Class         (MonadDB, MonadDBPure, getGStateDB)
import           Pos.DB.Error         (DBError (..))
import           Pos.DB.Functions     (RocksBatchOp (..), encodeWithKeyPrefix, rocksGetBi,
                                       rocksGetBytes)
import           Pos.DB.GState.Common (gsGetBi, gsPutBi, writeBatchGState)
import           Pos.DB.Iterator      (DBIteratorClass (..), DBnIterator, DBnMapIterator,
                                       IterType, runDBnIterator, runDBnMapIterator)
import           Pos.DB.Types         (DB, NodeDBs (_gStateDB))
import           Pos.Txp.Core         (TxIn (..), TxOutAux, addrBelongsToSet, txOutStake)
import           Pos.Txp.Toil.Types   (Utxo)
import           Pos.Types            (Address, Coin, coinF, mkCoin, sumCoins,
                                       unsafeAddCoin, unsafeIntegerToCoin)
import           Pos.Util.Iterator    (nextItem)

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

getTxOut :: MonadDBPure m => TxIn -> m (Maybe TxOutAux)
getTxOut = gsGetBi . txInKey

getTxOutFromDB :: (MonadIO m, MonadThrow m) => TxIn -> DB -> m (Maybe TxOutAux)
getTxOutFromDB txIn = rocksGetBi (txInKey txIn)

----------------------------------------------------------------------------
-- Batch operations
----------------------------------------------------------------------------

data UtxoOp
    = DelTxIn !TxIn
    | AddTxOut !TxIn !TxOutAux

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

prepareGStateUtxo :: MonadDB m => Utxo -> m ()
prepareGStateUtxo genesisUtxo =
    unlessM isUtxoInitialized putGenesisUtxo
  where
    putGenesisUtxo = do
        let utxoList = M.toList genesisUtxo
        writeBatchGState $ concatMap createBatchOp utxoList
        gsPutBi initializationFlagKey True
    createBatchOp (txin, txout) =
        [AddTxOut txin txout]

----------------------------------------------------------------------------
-- Iteration
----------------------------------------------------------------------------

data UtxoIter

instance DBIteratorClass UtxoIter where
    type IterKey UtxoIter = TxIn
    type IterValue UtxoIter = TxOutAux
    iterKeyPrefix _ = iterationUtxoPrefix

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
    => [Address] -> m Utxo
getFilteredUtxo' addrs = filterUtxo @i $ \(_, out) -> out `addrBelongsToSet` addrsSet
  where addrsSet = HS.fromList $ map AddressIA addrs

getFilteredUtxo :: MonadDB m => [Address] -> m Utxo
getFilteredUtxo = getFilteredUtxo' @UtxoIter

-- | Get full utxo. Use with care – the utxo can be very big (hundreds of
-- megabytes).
getAllPotentiallyHugeUtxo :: MonadDB m => m Utxo
getAllPotentiallyHugeUtxo = runUtxoIterator @UtxoIter (step mempty)
  where
    -- this can probably be written better
    step res = nextItem >>= \case
        Nothing     -> pure res
        Just (k, v) -> step (M.insert k v res)

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

initializationFlagKey :: ByteString
initializationFlagKey = "ut/gutxo/"

----------------------------------------------------------------------------
-- Details
----------------------------------------------------------------------------

isUtxoInitialized :: MonadDB m => m Bool
isUtxoInitialized = isJust <$> (getGStateDB >>= rocksGetBytes initializationFlagKey)
