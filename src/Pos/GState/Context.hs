-- | Data which is stored in memory and is related to GState.

module Pos.GState.Context
       ( GStateContext (..)
       , HasGStateContext (..)
       , DBSum (..)
       , eitherDB

       , cloneGStateContext
       , withClonedGState
       ) where

import           Universum

import           Control.Lens           (makeClassy)
import           System.Wlog            (WithLogger)
import           Ether.Internal         (HasLens (..))

import           Pos.Block.Slog.Context (SlogContext, cloneSlogContext)
import           Pos.DB.Pure            (DBPureVar, cloneDBPure)
import           Pos.DB.Rocks           (NodeDBs)
import           Pos.Lrc.Context        (LrcContext, cloneLrcContext)
import           Pos.Slotting           (SlottingData, cloneSlottingVar)

data DBSum = RealDB NodeDBs | PureDB DBPureVar

-- | This type contains DB and in-memory contexts which basically
-- replicate GState. It's parameterized by DB type, because we have
-- multiple DB implementations.
--
-- [CSL-1390] FIXME: add SSC GState here too!
data GStateContext = GStateContext
    { _gscDB          :: !DBSum
    , _gscLrcContext  :: !LrcContext
    , _gscSlogContext :: !SlogContext
    , _gscSlottingVar :: !(TVar SlottingData)
    }

makeClassy ''GStateContext

eitherDB
    :: (MonadReader ctx m, HasLens DBSum ctx DBSum)
    => ReaderT NodeDBs m a -> ReaderT DBPureVar m a -> m a
eitherDB ract pact = view (lensOf @DBSum) >>= \case
    RealDB dbs -> runReaderT ract dbs
    PureDB pdb -> runReaderT pact pdb

-- | Create a new 'GStateContext' which is a copy of the given context
-- and can be modified independently.
cloneGStateContext ::
       (MonadIO m, WithLogger m, MonadThrow m)
    => GStateContext
    -> m GStateContext
cloneGStateContext GStateContext {..} = case _gscDB of
    RealDB _ -> error "You may not copy RealDB" -- TODO maybe exception?
    PureDB pdb -> GStateContext . PureDB <$>
        cloneDBPure pdb <*>
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
