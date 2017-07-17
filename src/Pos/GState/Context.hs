-- | Data which is stored in memory and is related to GState.

module Pos.GState.Context
       ( GStateContext (..)
       , HasGStateContext (..)

       , GStateContextPure
       , cloneGStateContext
       , withClonedGState
       ) where

import           Universum

import           Control.Lens           (makeClassy)
import           System.Wlog            (WithLogger)

import           Pos.Block.Slog.Context (SlogContext, cloneSlogContext)
import           Pos.DB.Pure            (DBPureVar, cloneDBPure)
import           Pos.Lrc.Context        (LrcContext, cloneLrcContext)
import           Pos.Slotting           (SlottingData, cloneSlottingVar)

-- | This type contains DB and in-memory contexts which basically
-- replicate GState. It's parameterized by DB type, because we have
-- multiple DB implementations.
--
-- [CSL-1390] FIXME: add SSC GState here too!
data GStateContext db = GStateContext
    { _gscDB          :: !db
    , _gscLrcContext  :: !LrcContext
    , _gscSlogContext :: !SlogContext
    , _gscSlottingVar :: !(TVar SlottingData)
    }

makeClassy ''GStateContext

type GStateContextPure = GStateContext DBPureVar

-- | Create a new 'GStateContext' which is a copy of the given context
-- and can be modified independently.
cloneGStateContext ::
       (MonadIO m, WithLogger m, MonadThrow m)
    => GStateContextPure
    -> m GStateContextPure
cloneGStateContext GStateContext {..} =
    GStateContext <$> cloneDBPure _gscDB <*> cloneLrcContext _gscLrcContext <*>
    cloneSlogContext _gscSlogContext <*>
    cloneSlottingVar _gscSlottingVar

-- | Make a full copy of GState and run given action with this copy.
withClonedGState ::
       ( MonadIO m
       , WithLogger m
       , MonadThrow m
       , MonadReader ctx m
       , HasGStateContext ctx DBPureVar
       )
    => m a
    -> m a
withClonedGState action = do
    cloned <- cloneGStateContext =<< view gStateContext
    local (set gStateContext cloned) action
