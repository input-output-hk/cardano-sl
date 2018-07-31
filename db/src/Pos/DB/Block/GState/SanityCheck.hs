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
--import           Pos.Util.Log (WithLogger)
import           Pos.Util.Trace (noTrace)

sanityCheckDB ::
       ( MonadMask m
       , MonadDBRead m
       , MonadUnliftIO m
       )
    => m ()
sanityCheckDB = inAssertMode sanityCheckGStateDB

-- | Check that GState DB is consistent.
sanityCheckGStateDB ::
       forall m.
       ( MonadDBRead m
       , MonadUnliftIO m
       )
    => m ()
sanityCheckGStateDB = do
    sanityCheckStakes noTrace
    sanityCheckUtxo noTrace =<< getRealTotalStake
