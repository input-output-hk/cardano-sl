{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Part of GState DB which stores unspent transaction outputs.

module Pos.DB.Txp.Utxo
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
       , filterUtxo
       , getAllPotentiallyHugeUtxo

       -- * Sanity checks
       , sanityCheckUtxo
       ) where


import           Universum

import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Conduit (ConduitT, mapOutput, runConduitRes, (.|))
import qualified Data.Conduit.List as CL
import qualified Data.HashSet as HS
import qualified Data.Map as M
import qualified Database.RocksDB as Rocks
import           Formatting (bprint, build, sformat, (%))
import qualified Formatting.Buildable
import           Serokell.Util (Color (Red), colorize)
import           UnliftIO (MonadUnliftIO)

import           Pos.Binary.Class (serialize')
import           Pos.Chain.Txp (TxIn (..), TxOutAux (toaOut), Utxo,
                     addrBelongsToSet, genesisUtxo, txOutStake)
import           Pos.Core (Address, Coin, coinF, mkCoin, sumCoins,
                     unsafeAddCoin, unsafeIntegerToCoin)
import           Pos.Core.Genesis (GenesisData (..))
import           Pos.DB (DBError (..), DBIteratorClass (..), DBTag (GStateDB),
                     IterType, MonadDB, MonadDBRead, RocksBatchOp (..),
                     dbIterSource, encodeWithKeyPrefix)
import           Pos.DB.GState.Common (gsGetBi, writeBatchGState)
import           Pos.Util.Wlog (WithLogger, logError)

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

instance RocksBatchOp UtxoOp where
    toBatchOp (AddTxOut txIn txOut) =
        [Rocks.Put (txInKey txIn) (serialize' txOut)]
    toBatchOp (DelTxIn txIn) = [Rocks.Del $ txInKey txIn]

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

-- | Initializes utxo db.
initGStateUtxo :: (MonadDB m) => GenesisData -> m ()
initGStateUtxo genesisData =
    writeBatchGState $ concatMap createBatchOp utxoList
  where
    utxoList = M.toList $ genesisUtxo genesisData
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
utxoSource :: (MonadDBRead m) => ConduitT () (TxIn, TxOutAux) (ResourceT m) ()
utxoSource = dbIterSource GStateDB (Proxy @UtxoIter)

utxoSink :: (MonadDBRead m) => ConduitT (IterType UtxoIter) Void m Utxo
utxoSink = CL.fold (\u (k,v) -> M.insert k v u) mempty

-- | Retrieves only portion of UTXO related to given addresses list.
getFilteredUtxo :: (MonadDBRead m, MonadUnliftIO m) => [Address] -> m Utxo
getFilteredUtxo addrs = filterUtxo (\(_,out) -> out `addrBelongsToSet` addrsSet)
  where
    addrsSet = HS.fromList addrs

-- | Retrieves only portion of UTXO matching the given predicate.
filterUtxo :: (MonadDBRead m, MonadUnliftIO m) => ((TxIn, TxOutAux) -> Bool) -> m Utxo
filterUtxo predicate =
    runConduitRes $
    utxoSource .|
    CL.filter predicate .|
    utxoSink

-- | Get full utxo. Use with care – the utxo can be very big (hundreds of
-- megabytes).
getAllPotentiallyHugeUtxo :: (MonadDBRead m, MonadUnliftIO m) => m Utxo
getAllPotentiallyHugeUtxo = runConduitRes $ utxoSource .| utxoSink

----------------------------------------------------------------------------
-- Sanity checks
----------------------------------------------------------------------------

sanityCheckUtxo
    :: (MonadDBRead m, WithLogger m, MonadUnliftIO m)
    => GenesisData -> Coin -> m ()
sanityCheckUtxo genesisData expectedTotalStake = do
    let stakesSource =
            mapOutput (map snd . txOutStake (gdBootStakeholders genesisData)
                       . toaOut . snd) utxoSource
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
