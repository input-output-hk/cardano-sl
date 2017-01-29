-- | Main Toss logic.

module Pos.Ssc.GodTossing.Toss.Logic
       ( verifyAndApplyGtPayload
       , processGenesisBlock
       , rollbackGT
       , normalizeToss
       ) where

import           Control.Monad.Except            (MonadError)
import           Universum

import           Pos.Ssc.GodTossing.Core         (GtPayload)
import           Pos.Ssc.GodTossing.Toss.Class   (MonadToss, MonadTossRead)
import           Pos.Ssc.GodTossing.Toss.Failure (TossVerFailure (..))
import           Pos.Types                       (EpochIndex, MainBlockHeader)

-- | Verify 'GtPayload' with respect to data provided by
-- MonadToss. If data is valid it is also applied.  Otherwise
-- TossVerFailure is thrown using 'MonadError' type class.
verifyAndApplyGtPayload
    :: (MonadToss m, MonadError TossVerFailure m)
    => Either EpochIndex (MainBlockHeader ssc) -> GtPayload -> m ()
verifyAndApplyGtPayload _ _ = pass

-- | Process creation of genesis block for given epoch.
processGenesisBlock :: MonadToss m => EpochIndex -> m ()
processGenesisBlock _ = pass

-- | Rollback application of 'GtPayload' in 'Toss'.
rollbackGT :: MonadToss m => GtPayload -> m ()
rollbackGT _ = pass

-- | TODO: I am not sure about this function!
normalizeToss :: MonadTossRead m => GtPayload -> m ()
normalizeToss _ = pass
