-- | Functions for sanity checking the GState DB.

module Pos.DB.Block.GState.SanityCheck
       ( sanityCheckDB
       ) where

import           Universum

import           UnliftIO (MonadUnliftIO)

import           Pos.Chain.Genesis (GenesisData)
import           Pos.DB.Class (MonadDBRead)
import           Pos.DB.GState.Stakes (getRealTotalStake)
import           Pos.DB.Txp (sanityCheckStakes, sanityCheckUtxo)
import           Pos.Util.AssertMode (inAssertMode)
import           Pos.Util.Wlog (WithLogger)

sanityCheckDB ::
       ( MonadMask m
       , WithLogger m
       , MonadDBRead m
       , MonadUnliftIO m
       )
    => GenesisData -> m ()
sanityCheckDB = inAssertMode . sanityCheckGStateDB

-- | Check that GState DB is consistent.
sanityCheckGStateDB ::
       forall m.
       ( MonadDBRead m
       , MonadUnliftIO m
       , WithLogger m
       )
    => GenesisData
    -> m ()
sanityCheckGStateDB genesisData = do
    sanityCheckStakes
    sanityCheckUtxo genesisData =<< getRealTotalStake
