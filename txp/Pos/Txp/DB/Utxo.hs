{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Part of GState DB which stores unspent transaction outputs.

module Pos.Txp.DB.Utxo
       (
       -- * Getters
         getTxOut

       -- * Operations
       , UtxoOp (..)

       -- * Initialization
       , initGStateUtxo

       -- * Iteration
       , UtxoIter
       , utxoSource

       -- * Get utxo
       , getFilteredUtxo
       , getAllPotentiallyHugeUtxo

       -- * Sanity checks
       , sanityCheckUtxo
       ) where


import           Universum

import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Conduit (Sink, Source, mapOutput, runConduitRes, (.|))
import qualified Data.Conduit.List as CL
import qualified Data.HashSet as HS
import qualified Data.Map as M
import qualified Data.Text.Buildable
import qualified Database.RocksDB as Rocks
import           Formatting (bprint, build, sformat, (%))
import           Serokell.Util (Color (Red), colorize)
import           System.Wlog (WithLogger, logError)

import           Pos.Binary.Core ()
import           Pos.Core (Address, Coin, HasConfiguration, coinF, mkCoin, sumCoins, unsafeAddCoin,
                           unsafeIntegerToCoin)
import           Pos.Core.Txp (TxIn (..), TxOutAux (toaOut))
import           Pos.DB (DBError (..), DBIteratorClass (..), DBTag (GStateDB), IterType, MonadDB,
                         MonadDBRead, RocksBatchOp (..), dbIterSource, dbSerializeValue,
                         encodeWithKeyPrefix)
import           Pos.DB.GState.Common (gsGetBi, writeBatchGState)
import           Pos.Txp.Base (addrBelongsToSet, txOutStake)
import           Pos.Txp.Toil.Types (GenesisUtxo (..), Utxo)

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

getTxOut :: MonadDBRead m => TxIn -> m (Maybe TxOutAux)
getTxOut = gsGetBi . txInKey

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
        bprint ("AddTxOut ("%build%", "%build%")")
        txIn txOutAux

instance HasConfiguration => RocksBatchOp UtxoOp where
    toBatchOp (AddTxOut txIn txOut) =
        [Rocks.Put (txInKey txIn) (dbSerializeValue txOut)]
    toBatchOp (DelTxIn txIn) = [Rocks.Del $ txInKey txIn]

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

-- | Initializes utxo db.
initGStateUtxo :: (MonadDB m) => GenesisUtxo -> m ()
initGStateUtxo (GenesisUtxo genesisUtxo) =
    writeBatchGState $ concatMap createBatchOp utxoList
  where
    utxoList = M.toList genesisUtxo
    createBatchOp (txin, txout) = [AddTxOut txin txout]

----------------------------------------------------------------------------
-- Iteration
----------------------------------------------------------------------------

data UtxoIter

instance DBIteratorClass UtxoIter where
    type IterKey UtxoIter = TxIn
    type IterValue UtxoIter = TxOutAux
    iterKeyPrefix = iterationUtxoPrefix

-- | 'Source' corresponding to the whole utxo (inputs and outputs).
utxoSource :: (MonadDBRead m) => Source (ResourceT m) (TxIn, TxOutAux)
utxoSource = dbIterSource GStateDB (Proxy @UtxoIter)

utxoSink :: (MonadDBRead m) => Sink (IterType UtxoIter) m Utxo
utxoSink = CL.fold (\u (k,v) -> M.insert k v u) mempty

-- | Retrieves only portion of UTXO related to given addresses list.
getFilteredUtxo :: MonadDBRead m => [Address] -> m Utxo
getFilteredUtxo addrs =
    runConduitRes $
    utxoSource .|
    CL.filter (\(_,out) -> out `addrBelongsToSet` addrsSet) .|
    utxoSink
  where
    addrsSet = HS.fromList addrs

-- | Get full utxo. Use with care – the utxo can be very big (hundreds of
-- megabytes).
getAllPotentiallyHugeUtxo :: MonadDBRead m => m Utxo
getAllPotentiallyHugeUtxo = runConduitRes $ utxoSource .| utxoSink

----------------------------------------------------------------------------
-- Sanity checks
----------------------------------------------------------------------------

sanityCheckUtxo
    :: (MonadDBRead m, WithLogger m)
    => Coin -> m ()
sanityCheckUtxo expectedTotalStake = do
    let stakesSource =
            mapOutput (map snd . txOutStake . toaOut . snd) utxoSource
    calculatedTotalStake <-
        runConduitRes $ stakesSource .| CL.fold foldAdd (mkCoin 0)
    let fmt =
            ("Sum of stakes in Utxo differs from expected total stake (the former is "
             %coinF%", while the latter is "%coinF%")")
    let msg = sformat fmt calculatedTotalStake expectedTotalStake
    unless (calculatedTotalStake == expectedTotalStake) $ do
        logError $ colorize Red msg
        throwM $ DBMalformed msg
  where
    foldAdd acc stakes =
        acc `unsafeAddCoin` unsafeIntegerToCoin (sumCoins @[Coin] stakes)

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

txInKey :: TxIn -> ByteString
txInKey = encodeWithKeyPrefix @UtxoIter

iterationUtxoPrefix :: ByteString
iterationUtxoPrefix = "ut/t/"
