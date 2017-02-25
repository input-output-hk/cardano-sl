{-# LANGUAGE ScopedTypeVariables #-}

-- | Higher-level functions working with GState DB.

module Pos.DB.GState.GState
       ( prepareGStateDB
       , sanityCheckGStateDB
       , usingGStateSnapshot
       ) where

import           Control.Monad.Catch    (MonadMask)
import qualified Database.RocksDB       as Rocks
import           System.Wlog            (WithLogger)
import           Universum

import           Pos.Context.Class      (WithNodeContext (getNodeContext))
import           Pos.Context.Context    (ncSystemStart)
import           Pos.Context.Functions  (genesisUtxoM)
import           Pos.DB.Class           (MonadDB (getNodeDBs, usingReadOptions))
import           Pos.DB.GState.Balances (getTotalFtsStake, prepareGStateBalances,
                                         sanityCheckBalances)
import           Pos.DB.GState.Common   (prepareGStateCommon)
import           Pos.DB.GState.Update   (prepareGStateUS)
import           Pos.DB.GState.Utxo     (prepareGStateUtxo, sanityCheckUtxo)
import           Pos.DB.Types           (DB (..), NodeDBs (..), Snapshot (..), gStateDB,
                                         usingSnapshot)
import           Pos.Types              (HeaderHash)

-- | Put missing initial data into GState DB.
prepareGStateDB
    :: forall ssc m.
       (WithNodeContext ssc m, MonadDB ssc m)
    => HeaderHash -> m ()
prepareGStateDB initialTip = do
    prepareGStateCommon initialTip
    genesisUtxo <- genesisUtxoM
    prepareGStateUtxo genesisUtxo
    prepareGStateBalances genesisUtxo
    systemStart <- ncSystemStart <$> getNodeContext
    prepareGStateUS systemStart

-- | Check that GState DB is consistent.
sanityCheckGStateDB
    :: forall ssc m.
       (MonadDB ssc m, MonadMask m, WithLogger m)
    => m ()
sanityCheckGStateDB = do
    sanityCheckBalances
    sanityCheckUtxo =<< getTotalFtsStake

usingGStateSnapshot :: (MonadDB ssc m, MonadMask m) => m a -> m a
usingGStateSnapshot action = do
    db <- _gStateDB <$> getNodeDBs
    let readOpts = rocksReadOpts db
    usingSnapshot db (\(Snapshot sn) ->
        usingReadOptions readOpts {Rocks.useSnapshot = Just sn} gStateDB action)
