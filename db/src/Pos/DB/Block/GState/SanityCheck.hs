-- | Functions for sanity checking the GState DB.

module Pos.DB.Block.GState.SanityCheck
       ( sanityCheckDB
       ) where

import           Universum

import           UnliftIO (MonadUnliftIO)

import           Pos.DB.Class (MonadDBRead)
import           Pos.DB.GState.Stakes (getRealTotalStake)
import           Pos.DB.Txp (sanityCheckStakes, sanityCheckUtxo)
import           Pos.Util.AssertMode (inAssertMode)
import           Pos.Util.Trace.Named (TraceNamed, natTrace)

sanityCheckDB ::
       ( MonadMask m
       , MonadDBRead m
       , MonadUnliftIO m
       )
    => TraceNamed IO -> m ()
sanityCheckDB logTrace = inAssertMode $ sanityCheckGStateDB $ natTrace liftIO logTrace

-- | Check that GState DB is consistent.
sanityCheckGStateDB ::
       forall m.
       ( MonadDBRead m
       , MonadUnliftIO m
       )
    => TraceNamed m -> m ()
sanityCheckGStateDB logTrace = do
    sanityCheckStakes logTrace
    sanityCheckUtxo logTrace =<< getRealTotalStake
