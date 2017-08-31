-- | Functions operation on 'SlogContext' and its subtypes.

module Pos.Block.Slog.Context
       ( SlogGState (..)
       , SlogContext (..)

       , mkSlogGState
       , mkSlogContext
       , cloneSlogGState
       , slogGetLastSlots
       , slogPutLastSlots
       ) where

import           Universum

import           Formatting            (int, sformat, (%))
import qualified System.Metrics        as Ekg

import           Pos.Block.Slog.Types  (HasSlogGState (..), LastBlkSlots,
                                        SlogContext (..), SlogGState (..),
                                        sgsLastBlkSlots)
import           Pos.Core              (HasCoreConstants, blkSecurityParam)
import           Pos.DB.Class          (MonadDBRead)
import           Pos.GState.BlockExtra (getLastSlots)
import           Pos.Reporting         (mkDistrMonitorState)

-- | Make new 'SlogGState' using data from DB.
mkSlogGState :: (MonadIO m, MonadDBRead m) => m SlogGState
mkSlogGState = do
    _sgsLastBlkSlots <- getLastSlots >>= newIORef
    return SlogGState {..}

-- | Make new 'SlogContext' using data from DB.
mkSlogContext ::
       (MonadIO m, MonadDBRead m, HasCoreConstants)
    => Maybe Ekg.Store
    -> m SlogContext
mkSlogContext storeMaybe = do
    _scGState <- mkSlogGState
    let metricNameK =
            sformat ("Chain quality for last k ("%int%") blocks")
                blkSecurityParam
    let metricNameOverall = "Overall chain quality"
    _scCQkMonitorState <- mkDistrMonitorState metricNameK storeMaybe
    _scCQOverallMonitorState <- mkDistrMonitorState metricNameOverall storeMaybe
    return SlogContext {..}

-- | Make a copy of existing 'SlogGState'.
cloneSlogGState :: (MonadIO m) => SlogGState -> m SlogGState
cloneSlogGState SlogGState {..} =
    SlogGState <$> (readIORef _sgsLastBlkSlots >>= newIORef)

-- | Read 'LastBlkSlots' from in-memory state.
slogGetLastSlots ::
       (MonadReader ctx m, HasSlogGState ctx, MonadIO m) => m LastBlkSlots
slogGetLastSlots = view (slogGState . sgsLastBlkSlots) >>= readIORef

-- | Update 'LastBlkSlots' in 'SlogContext'.
slogPutLastSlots ::
       (MonadReader ctx m, HasSlogGState ctx, MonadIO m)
    => LastBlkSlots
    -> m ()
slogPutLastSlots slots =
    view (slogGState . sgsLastBlkSlots) >>= flip writeIORef slots
