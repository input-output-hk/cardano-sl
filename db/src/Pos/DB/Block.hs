{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Interface and implementation of Blocks DB: storing blocks in files on disk.

module Pos.DB.Block
       ( getBlock
       , deleteBlock
       , consolidateWorker

       , prepareBlockDB

       -- * Pure implementation
       , dbGetSerBlockPureDefault
       , dbGetSerUndoPureDefault
       , dbGetSerBlundPureDefault
       , dbPutSerBlundsPureDefault

       -- * Rocks implementation
       , dbGetSerBlockRealDefault
       , dbGetSerUndoRealDefault
       , dbGetSerBlundRealDefault
       , dbPutSerBlundsRealDefault

       -- * DBSum implementation
       , dbGetSerBlockSumDefault
       , dbGetSerUndoSumDefault
       , dbGetSerBlundSumDefault
       , dbPutSerBlundsSumDefault

       , module X
       ) where

import           Universum

import           Pos.Chain.Block (HeaderHash)
import qualified Pos.Chain.Block as CB
import           Pos.Chain.Genesis (GenesisHash)
import           Pos.DB.Block.Epoch (consolidateWorker,
                     dbGetConsolidatedSerBlockRealDefault,
                     dbGetConsolidatedSerBlundRealDefault,
                     dbGetConsolidatedSerUndoRealDefault)
import           Pos.DB.Block.Internal (dbGetSerBlockPureDefault,
                     dbGetSerBlundPureDefault, dbGetSerUndoPureDefault,
                     dbPutSerBlundsPureDefault, dbPutSerBlundsRealDefault,
                     deleteBlock, prepareBlockDB)
import           Pos.DB.Class (MonadDB (..), MonadDBRead (..), SerializedBlock,
                     SerializedBlund, SerializedUndo, getBlock)
import           Pos.DB.Rocks (MonadRealDB)
import           Pos.DB.Sum (MonadDBSum, eitherDB)

import           Pos.DB.Block.BListener as X
import           Pos.DB.Block.GState.BlockExtra as X
import           Pos.DB.Block.Load as X
import           Pos.DB.Block.Logic.Creation as X
import           Pos.DB.Block.Logic.Header as X
import           Pos.DB.Block.Logic.Internal as X
import           Pos.DB.Block.Logic.SplitByEpoch as X
import           Pos.DB.Block.Logic.Util as X
import           Pos.DB.Block.Logic.VAR as X
import           Pos.DB.Block.Lrc as X
import           Pos.DB.Block.Slog.Context as X
import           Pos.DB.Block.Slog.Logic as X


-- | Switch out the default 'dbGetSerBlundRealDefault' with the one that can
-- get a 'SerializedBlund' from either an epoch file (if the blund has already
-- been consolidated) or a regular blund file if it has not.
dbGetSerBlundRealDefault
    :: (MonadDBRead m, MonadRealDB ctx m)
    => GenesisHash
    -> HeaderHash
    -> m (Maybe SerializedBlund)
dbGetSerBlundRealDefault = dbGetConsolidatedSerBlundRealDefault

dbGetSerBlockRealDefault
    :: (MonadDBRead m, MonadRealDB ctx m)
    => GenesisHash
    -> HeaderHash
    -> m (Maybe SerializedBlock)
dbGetSerBlockRealDefault = dbGetConsolidatedSerBlockRealDefault

dbGetSerUndoRealDefault
    :: (MonadDBRead m, MonadRealDB ctx m)
    => GenesisHash
    -> HeaderHash
    -> m (Maybe SerializedUndo)
dbGetSerUndoRealDefault = dbGetConsolidatedSerUndoRealDefault

----------------------------------------------------------------------------
-- DBSum implementation
----------------------------------------------------------------------------

type DBSumEnv ctx m =
    ( MonadDB m
    , MonadDBSum ctx m
    )

dbGetSerBlockSumDefault
    :: forall ctx m. (DBSumEnv ctx m)
    => GenesisHash -> HeaderHash -> m (Maybe SerializedBlock)
dbGetSerBlockSumDefault genesisHash hh = eitherDB
    (dbGetSerBlockRealDefault genesisHash hh)
    (dbGetSerBlockPureDefault hh)

dbGetSerUndoSumDefault
    :: forall ctx m. DBSumEnv ctx m
    => GenesisHash -> HeaderHash -> m (Maybe SerializedUndo)
dbGetSerUndoSumDefault genesisHash hh = eitherDB
    (dbGetSerUndoRealDefault genesisHash hh)
    (dbGetSerUndoPureDefault hh)

dbGetSerBlundSumDefault
    :: forall ctx m. DBSumEnv ctx m
    => GenesisHash -> HeaderHash -> m (Maybe SerializedBlund)
dbGetSerBlundSumDefault genesisHash hh = eitherDB
    (dbGetSerBlundRealDefault genesisHash hh)
    (dbGetSerBlundPureDefault hh)

dbPutSerBlundsSumDefault
    :: forall ctx m. (DBSumEnv ctx m)
    => NonEmpty (CB.BlockHeader, SerializedBlund) -> m ()
dbPutSerBlundsSumDefault b =
    eitherDB (dbPutSerBlundsRealDefault b) (dbPutSerBlundsPureDefault b)
