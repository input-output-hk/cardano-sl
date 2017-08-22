-- | Data which is stored in memory and is related to GState.

module Pos.GState.Context
       ( GStateContext (..)
       , HasGStateContext (..)

       , cloneGStateContext
       , withClonedGState
       ) where

import           Universum

import           Control.Lens           (makeClassy)
import           System.Wlog            (WithLogger)

import           Pos.Block.Slog.Context (SlogContext, cloneSlogContext)
import           Pos.DB.Pure            (cloneDBPure)
import           Pos.DB.Sum             (DBSum (..))
import           Pos.Lrc.Context        (LrcContext, cloneLrcContext)
import           Pos.Slotting           (SlottingData, cloneSlottingVar)

-- | This type contains DB and in-memory contexts which basically
-- replicate GState. It's parameterized by DB type, because we have
-- multiple DB implementations.
--
-- [CSL-1390] FIXME: add SSC GState here too!
data GStateContext = GStateContext
    { _gscDB          :: !DBSum
    , _gscLrcContext  :: LrcContext
    , _gscSlogContext :: SlogContext
    , _gscSlottingVar :: TVar SlottingData
    -- Fields are lazy to be used with future.
    }

makeClassy ''GStateContext

-- | Create a new 'GStateContext' which is a copy of the given context
-- and can be modified independently.
cloneGStateContext ::
       (MonadIO m, WithLogger m, MonadThrow m)
    => GStateContext
    -> m GStateContext
cloneGStateContext GStateContext {..} = case _gscDB of
    RealDB _ -> error "You may not copy RealDB" -- TODO maybe exception?
    PureDB pdb -> GStateContext <$>
        (PureDB <$> cloneDBPure pdb) <*>
        cloneLrcContext _gscLrcContext <*>
        cloneSlogContext _gscSlogContext <*>
        cloneSlottingVar _gscSlottingVar

-- | Make a full copy of GState and run given action with this copy.
withClonedGState ::
       ( MonadIO m
       , WithLogger m
       , MonadThrow m
       , MonadReader ctx m
       , HasGStateContext ctx
       )
    => m a
    -> m a
withClonedGState action = do
    cloned <- cloneGStateContext =<< view gStateContext
    local (set gStateContext cloned) action
