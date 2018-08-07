-- | Higher-level functions working with GState DB.

module Pos.GState.GState
       ( prepareGStateDB
       ) where

import           Universum

import           Pos.Chain.Block (HeaderHash)
import           Pos.Chain.Txp (genesisUtxo)
import           Pos.Core (genesisData)
import           Pos.Core.Genesis (gdHeavyDelegation)
import           Pos.DB.Block (initGStateBlockExtra)
import           Pos.DB.Class (MonadDB)
import           Pos.DB.Delegation (initGStateDlg)
import           Pos.DB.GState.Common (initGStateCommon, isInitialized,
                     setInitialized)
import           Pos.DB.Ssc (initSscDB)
import           Pos.DB.Txp (initGStateStakes, initGStateUtxo)
import           Pos.DB.Update (initGStateUS)

-- | Put missing initial data into GState DB.
prepareGStateDB ::
       forall ctx m.
       ( MonadReader ctx m
       , MonadDB m
       )
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

-- The following is not used in the project yet. To be added back at a
-- later stage when needed.

{-
usingGStateSnapshot :: (MonadRealDB ctx m, MonadMask m) => m a -> m a
usingGStateSnapshot action = do
    db <- _gStateDB <$> getNodeDBs
    let readOpts = rocksReadOpts db
    usingSnapshot db (\(Snapshot sn) ->
        usingReadOptions readOpts {Rocks.useSnapshot = Just sn} gStateDB action)
-}
