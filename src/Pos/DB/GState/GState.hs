{-# LANGUAGE ScopedTypeVariables #-}

-- | Higher-level functions working with GState DB.

module Pos.DB.GState.GState
       ( prepareGStateDB
       , sanityCheckGStateDB
       , usingGStateSnapshot
       ) where

import           Universum

import           Control.Monad.Catch        (MonadMask)
import qualified Database.RocksDB           as Rocks
import qualified Ether
import           System.Wlog                (WithLogger)

import           Pos.Context.Context        (GenesisUtxo (..), NodeParams (..))
import           Pos.Context.Functions      (genesisUtxoM)
import           Pos.DB.Class               (MonadDB, MonadDBPure, getNodeDBs,
                                             usingReadOptions)
import           Pos.DB.GState.Balances     (getRealTotalStake)
import           Pos.DB.GState.Common       (prepareGStateCommon)
import           Pos.DB.Types               (DB (..), NodeDBs (..), Snapshot (..),
                                             gStateDB, usingSnapshot)
import           Pos.Ssc.GodTossing.DB      (prepareGtDB)
import           Pos.Ssc.GodTossing.Genesis (genesisCertificates)
import           Pos.Txp.DB                 (prepareGStateBalances, prepareGStateUtxo,
                                             sanityCheckBalances, sanityCheckUtxo)
import           Pos.Types                  (HeaderHash)
import           Pos.Update.DB              (prepareGStateUS)

-- | Put missing initial data into GState DB.
prepareGStateDB
    :: forall m.
       ( Ether.MonadReader' NodeParams m
       , Ether.MonadReader' GenesisUtxo m
       , MonadDB m
       , MonadDBPure m)
    => HeaderHash -> m ()
prepareGStateDB initialTip = do
    prepareGStateCommon initialTip
    genesisUtxo <- genesisUtxoM
    prepareGStateUtxo genesisUtxo
    prepareGtDB genesisCertificates
    prepareGStateBalances genesisUtxo
    systemStart <- Ether.asks' npSystemStart
    prepareGStateUS systemStart

-- | Check that GState DB is consistent.
sanityCheckGStateDB
    :: forall m.
       (MonadDB m, MonadDBPure m, MonadMask m, WithLogger m)
    => m ()
sanityCheckGStateDB = do
    sanityCheckBalances
    sanityCheckUtxo =<< getRealTotalStake

usingGStateSnapshot :: (MonadDB m, MonadMask m) => m a -> m a
usingGStateSnapshot action = do
    db <- _gStateDB <$> getNodeDBs
    let readOpts = rocksReadOpts db
    usingSnapshot db (\(Snapshot sn) ->
        usingReadOptions readOpts {Rocks.useSnapshot = Just sn} gStateDB action)
