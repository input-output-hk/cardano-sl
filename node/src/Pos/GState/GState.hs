{-# LANGUAGE CPP #-}

-- | Higher-level functions working with GState DB.

module Pos.GState.GState
       ( prepareGStateDB
       , sanityCheckGStateDB
       , usingGStateSnapshot
       ) where

import           Universum

import           Control.Monad.Catch    (MonadMask)
import qualified Database.RocksDB       as Rocks
import           System.Wlog            (WithLogger)

import           Pos.Core               (GenesisWStakeholders, HasCoreConstants,
                                         HeaderHash)
import           Pos.DB.Class           (MonadDB, MonadDBRead)
import           Pos.DB.GState.Stakes   (getRealTotalStake)
import           Pos.DB.GState.Common   (initGStateCommon, isInitialized, setInitialized)
import           Pos.DB.Rocks           (DB (..), MonadRealDB, NodeDBs (..),
                                         Snapshot (..), gStateDB, getNodeDBs,
                                         usingReadOptions, usingSnapshot)
import           Pos.Delegation.DB      (initGStateDlg)
import           Pos.Genesis            (GenesisContext, gtcDelegation, gtcUtxo,
                                         gtcWStakeholders)
import           Pos.GState.BlockExtra  (initGStateBlockExtra)
import           Pos.Ssc.GodTossing.DB  (initGtDB)
import           Pos.Txp.DB             (initGStateStakes, initGStateUtxo,
                                         sanityCheckStakes, sanityCheckUtxo)
import           Pos.Update.DB          (initGStateUS)
import           Pos.Util.Util          (HasLens', lensOf')

#ifdef WITH_EXPLORER
import qualified Pos.Explorer.DB        as ExplorerDB
#endif

-- | Put missing initial data into GState DB.
prepareGStateDB ::
       forall ctx m.
       ( MonadReader ctx m
       , HasLens' ctx GenesisContext
       , MonadDB m
       , HasCoreConstants)
    => HeaderHash
    -> m ()
prepareGStateDB initialTip = unlessM isInitialized $ do
    genesisUtxo <- view (lensOf' . gtcUtxo)
    genesisWStakeholders <- view (lensOf' . gtcWStakeholders)
    genesisDelegation <- view (lensOf' . gtcDelegation)

    initGStateCommon initialTip
    initGStateUtxo genesisUtxo
    initGtDB
    initGStateStakes genesisUtxo genesisWStakeholders
    initGStateUS
    initGStateDlg genesisDelegation
    initGStateBlockExtra initialTip

    setInitialized

-- | Check that GState DB is consistent.
sanityCheckGStateDB ::
       forall ctx m.
       ( MonadDBRead m
       , MonadMask m
       , WithLogger m
       , MonadReader ctx m
       , HasLens' ctx GenesisWStakeholders
       )
    => m ()
sanityCheckGStateDB = do
    sanityCheckStakes
    sanityCheckUtxo =<< getRealTotalStake
#ifdef WITH_EXPLORER
    ExplorerDB.sanityCheckBalances
#endif

usingGStateSnapshot :: (MonadRealDB ctx m, MonadMask m) => m a -> m a
usingGStateSnapshot action = do
    db <- _gStateDB <$> getNodeDBs
    let readOpts = rocksReadOpts db
    usingSnapshot db (\(Snapshot sn) ->
        usingReadOptions readOpts {Rocks.useSnapshot = Just sn} gStateDB action)
