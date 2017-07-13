-- | Functions operation on 'SlogContext'.

module Pos.Block.Slog.Context
       ( mkSlogContext
       , slogGetLastSlots
       , slogPutLastSlots
       ) where

import           Universum

import           Pos.Block.Slog.Types (HasSlogContext (..), LastBlkSlots,
                                       SlogContext (..), scLastBlkSlots)
import           Pos.DB.Class         (MonadDBRead)
import           Pos.DB.GState        (getLastSlots)

-- | Make new 'SlogContext' using data from DB.
mkSlogContext :: (MonadIO m, MonadDBRead m) => m SlogContext
mkSlogContext = do
    _scLastBlkSlots <- getLastSlots >>= newIORef
    return SlogContext {..}

-- | Read 'LastBlkSlots' from in-memory state.
slogGetLastSlots ::
       (MonadReader ctx m, HasSlogContext ctx, MonadIO m) => m LastBlkSlots
slogGetLastSlots = view (slogContextL . scLastBlkSlots) >>= readIORef

-- | Update 'LastBlkSlots' in 'SlogContext'.
slogPutLastSlots ::
       (MonadReader ctx m, HasSlogContext ctx, MonadIO m)
    => LastBlkSlots
    -> m ()
slogPutLastSlots slots =
    view (slogContextL . scLastBlkSlots) >>= flip writeIORef slots
