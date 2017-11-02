{-# LANGUAGE CPP #-}

-- | Higher-level functions working with GState DB.

module Pos.GState.GState
       ( prepareGStateDB
       , sanityCheckGStateDB
       , usingGStateSnapshot
       ) where

import           Universum

import           Control.Monad.Catch              (MonadMask)
import qualified Database.RocksDB                 as Rocks
import           System.Wlog                      (WithLogger)

import           Pos.Core                         (GenesisData (..), HasConfiguration,
                                                   HeaderHash, genesisData)
import           Pos.DB.Class                     (MonadDB, MonadDBRead)
import           Pos.DB.GState.Common             (initGStateCommon, isInitialized,
                                                   setInitialized)
import           Pos.DB.GState.Stakes             (getRealTotalStake)
import           Pos.DB.Rocks                     (DB (..), MonadRealDB, NodeDBs (..),
                                                   Snapshot (..), gStateDB, getNodeDBs,
                                                   usingReadOptions, usingSnapshot)
import           Pos.Delegation.DB                (initGStateDlg)
import           Pos.GState.BlockExtra            (initGStateBlockExtra)
import           Pos.Ssc.Configuration            (HasSscConfiguration)
import           Pos.Ssc.DB                       (initSscDB)
import           Pos.Txp.DB                       (initGStateStakes, initGStateUtxo,
                                                   sanityCheckStakes, sanityCheckUtxo)
import           Pos.Txp.GenesisUtxo              (genesisUtxo)
import           Pos.Update.DB                    (initGStateUS)

-- | Put missing initial data into GState DB.
prepareGStateDB ::
       forall ctx m.
       ( MonadReader ctx m
       , MonadDB m
       , HasConfiguration
       , HasSscConfiguration)
    => HeaderHash
    -> m ()
prepareGStateDB initialTip = unlessM isInitialized $ do
    initGStateCommon initialTip
    initGStateUtxo genesisUtxo
    initSscDB
    initGStateStakes genesisUtxo
    initGStateUS
    initGStateDlg $ gdHeavyDelegation genesisData
    initGStateBlockExtra initialTip

    setInitialized

-- | Check that GState DB is consistent.
sanityCheckGStateDB ::
       forall ctx m.
       ( MonadDBRead m
       , MonadMask m
       , WithLogger m
       , MonadReader ctx m
       )
    => m ()
sanityCheckGStateDB = do
    sanityCheckStakes
    sanityCheckUtxo =<< getRealTotalStake

usingGStateSnapshot :: (MonadRealDB ctx m, MonadMask m) => m a -> m a
usingGStateSnapshot action = do
    db <- _gStateDB <$> getNodeDBs
    let readOpts = rocksReadOpts db
    usingSnapshot db (\(Snapshot sn) ->
        usingReadOptions readOpts {Rocks.useSnapshot = Just sn} gStateDB action)
