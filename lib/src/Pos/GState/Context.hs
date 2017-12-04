-- | Data which is stored in memory and is related to GState.

module Pos.GState.Context
       ( GStateContext (..)
       , HasGStateContext (..)

       , getGStateImplicit
       , cloneGStateContext
       , withClonedGState
       ) where

import           Universum

import           Control.Lens (lens, makeClassy)
import           Ether.Internal (lensOf)
import           System.Wlog (WithLogger)

import           Pos.Block.Slog (HasSlogGState (..), SlogGState, cloneSlogGState)
import           Pos.DB.Pure (cloneDBPure)
import           Pos.DB.Sum (DBSum (..))
import           Pos.Lrc.Context (HasLrcContext, LrcContext, cloneLrcContext)
import           Pos.Slotting (HasSlottingVar, SlottingVar, cloneSlottingVar, slottingVar)
import           Pos.Util (HasLens')

-- | This type contains DB and in-memory contexts which basically
-- replicate GState. It's parameterized by DB type, because we have
-- multiple DB implementations.
--
-- [CSL-1390] FIXME: add SSC GState here too!
data GStateContext = GStateContext
    { _gscDB          :: !DBSum
    , _gscLrcContext  :: LrcContext
    , _gscSlogGState  :: SlogGState
    , _gscSlottingVar :: SlottingVar
    -- Fields are lazy to be used with future.
    }

makeClassy ''GStateContext

instance HasSlogGState GStateContext where
    slogGState = gscSlogGState

-- | Constructs 'GStateContext' out of real components. Basically a
-- vertical lens sum.
getGStateImplicit ::
       ( HasSlottingVar ctx
       , HasSlogGState ctx
       , HasLens' ctx DBSum
       , HasLrcContext ctx)
    => Lens' ctx GStateContext
getGStateImplicit = lens getter setter
  where
    getter ctx =
        GStateContext
            (ctx ^. (lensOf @DBSum))
            (ctx ^. (lensOf @LrcContext))
            (ctx ^. slogGState)
            (ctx ^. slottingVar)
    setter ctx GStateContext{..} =
        ctx & (lensOf @DBSum) .~ _gscDB
            & (lensOf @LrcContext) .~ _gscLrcContext
            & slogGState .~ _gscSlogGState
            & slottingVar .~ _gscSlottingVar

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
        cloneSlogGState _gscSlogGState <*>
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
