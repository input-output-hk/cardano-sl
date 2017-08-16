{-# LANGUAGE ScopedTypeVariables #-}

-- | Higher-level functions working with GState DB.

module Pos.GState.GState
       ( prepareGStateDB
       , sanityCheckGStateDB
       , usingGStateSnapshot
       ) where

import           Universum

import           Control.Monad.Catch    (MonadMask)
import qualified Database.RocksDB       as Rocks
import           Ether.Internal         (HasLens (..))
import           System.Wlog            (WithLogger)

import           Pos.Context.Functions  (genesisUtxoM)
import           Pos.Core               (HeaderHash)
import           Pos.DB.Class           (MonadDB, MonadDBRead)
import           Pos.DB.GState.Balances (getRealTotalStake)
import           Pos.DB.GState.Common   (initGStateCommon, isInitialized, setInitialized)
import           Pos.DB.Rocks           (DB (..), MonadRealDB, NodeDBs (..),
                                         Snapshot (..), gStateDB, getNodeDBs,
                                         usingReadOptions, usingSnapshot)
import           Pos.Genesis            (GenesisUtxo (..))
import           Pos.GState.BlockExtra  (initGStateBlockExtra)
import           Pos.Ssc.GodTossing.DB  (initGtDB)
import           Pos.Txp.DB             (initGStateBalances, initGStateUtxo,
                                         sanityCheckBalances, sanityCheckUtxo)
import           Pos.Update.DB          (initGStateUS)

-- | Put missing initial data into GState DB.
prepareGStateDB ::
       forall ctx m.
       (MonadReader ctx m, HasLens GenesisUtxo ctx GenesisUtxo, MonadDB m)
    => HeaderHash
    -> m ()
prepareGStateDB initialTip = unlessM isInitialized $ do
    genesisUtxo <- genesisUtxoM

    initGStateCommon initialTip
    initGStateUtxo genesisUtxo
    initGtDB
    initGStateBalances genesisUtxo
    initGStateUS
    initGStateBlockExtra initialTip

    setInitialized

-- | Check that GState DB is consistent.
sanityCheckGStateDB
    :: forall m.
       (MonadDBRead m, MonadMask m, WithLogger m)
    => m ()
sanityCheckGStateDB = do
    sanityCheckBalances
    sanityCheckUtxo =<< getRealTotalStake

usingGStateSnapshot :: (MonadRealDB ctx m, MonadMask m) => m a -> m a
usingGStateSnapshot action = do
    db <- _gStateDB <$> getNodeDBs
    let readOpts = rocksReadOpts db
    usingSnapshot db (\(Snapshot sn) ->
        usingReadOptions readOpts {Rocks.useSnapshot = Just sn} gStateDB action)
