-- | Main Toss logic.

module Pos.Ssc.GodTossing.Toss.Logic
       ( verifyAndApplyGtPayload
       , rollbackGT
       , normalizeToss
       ) where

import           Control.Monad.Except            (MonadError)
import           Universum

import           Pos.Ssc.GodTossing.Core         (GtPayload)
import           Pos.Ssc.GodTossing.Toss.Class   (MonadToss)
import           Pos.Ssc.GodTossing.Toss.Failure (TossVerFailure (..))
import           Pos.Ssc.GodTossing.Toss.Types   (TossModifier)
import           Pos.Types                       (EpochIndex, MainBlockHeader)

-- | Verify 'GtPayload' with respect to data provided by
-- MonadToss. If data is valid it is also applied.  Otherwise
-- TossVerFailure is thrown using 'MonadError' type class.
verifyAndApplyGtPayload
    :: (MonadToss m, MonadError TossVerFailure m)
    => Either EpochIndex (MainBlockHeader ssc) -> GtPayload -> m ()
verifyAndApplyGtPayload _ _ = const pass notImplemented

-- | Rollback application of 'GtPayload' in 'Toss'.
rollbackGT :: MonadToss m => GtPayload -> m ()
rollbackGT _ = const pass notImplemented

-- | Apply as much data from given 'TossModifier' as possible.
normalizeToss
    :: MonadToss m
    => EpochIndex -> TossModifier -> m ()
normalizeToss _ _ = const pass notImplemented
