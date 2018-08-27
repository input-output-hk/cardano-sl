{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Interface and implementation of Blocks DB: storing blocks in files on disk.

module Pos.DB.Block
       ( getBlock
       , deleteBlock

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
import           Pos.DB.Block.Internal (dbGetSerBlockPureDefault,
                     dbGetSerBlockRealDefault, dbGetSerBlundPureDefault,
                     dbGetSerBlundRealDefault, dbGetSerUndoPureDefault,
                     dbGetSerUndoRealDefault, dbPutSerBlundsPureDefault,
                     dbPutSerBlundsRealDefault, deleteBlock, prepareBlockDB)
import           Pos.DB.Class (MonadDB (..), SerializedBlock, SerializedBlund,
                     SerializedUndo, getBlock)
import           Pos.DB.Sum (MonadDBSum, eitherDB)

import           Pos.DB.Block.BListener as X
import           Pos.DB.Block.GState.BlockExtra as X
import           Pos.DB.Block.Load as X
import           Pos.DB.Block.Logic.Creation as X
import           Pos.DB.Block.Logic.Header as X
import           Pos.DB.Block.Logic.Internal as X
import           Pos.DB.Block.Logic.Util as X
import           Pos.DB.Block.Logic.VAR as X
import           Pos.DB.Block.Lrc as X
import           Pos.DB.Block.Slog.Context as X
import           Pos.DB.Block.Slog.Logic as X

----------------------------------------------------------------------------
-- DBSum implementation
----------------------------------------------------------------------------

type DBSumEnv ctx m =
    ( MonadDB m
    , MonadDBSum ctx m
    )

dbGetSerBlockSumDefault
    :: forall ctx m. (DBSumEnv ctx m)
    => HeaderHash -> m (Maybe SerializedBlock)
dbGetSerBlockSumDefault hh = eitherDB (dbGetSerBlockRealDefault hh) (dbGetSerBlockPureDefault hh)

dbGetSerUndoSumDefault
    :: forall ctx m. DBSumEnv ctx m
    => HeaderHash -> m (Maybe SerializedUndo)
dbGetSerUndoSumDefault hh =
    eitherDB (dbGetSerUndoRealDefault hh) (dbGetSerUndoPureDefault hh)

dbGetSerBlundSumDefault
    :: forall ctx m. DBSumEnv ctx m
    => HeaderHash -> m (Maybe SerializedBlund)
dbGetSerBlundSumDefault hh =
    eitherDB (dbGetSerBlundRealDefault hh) (dbGetSerBlundPureDefault hh)

dbPutSerBlundsSumDefault
    :: forall ctx m. (DBSumEnv ctx m)
    => NonEmpty (CB.BlockHeader, SerializedBlund) -> m ()
dbPutSerBlundsSumDefault b =
    eitherDB (dbPutSerBlundsRealDefault b) (dbPutSerBlundsPureDefault b)
