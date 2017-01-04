-- | Major logic of Update System (US).

module Pos.Update.Logic
       ( usApplyBlocks
       , usRollbackBlocks
       , usVerifyBlocks
       ) where

import           Serokell.Util.Verify (VerificationRes)
import           Universum

import           Pos.Types            (NEBlocks)
import           Pos.WorkMode         (WorkMode)

-- | Apply chain of /definitely/ valid blocks to US part of ChainState
-- DB and to US local data. Head must be the __oldest__ block.
--
-- FIXME: return Batch.
usApplyBlocks :: WorkMode ssc m => NEBlocks ssc -> m ()
usApplyBlocks _ = pass

-- | Revert application of given blocks to US part of ChainState DB
-- and US local data. Head must be the __youngest__ block. Caller must
-- ensure that tip stored in DB is 'headerHash' of head.
--
-- FIXME: return Batch.
usRollbackBlocks :: WorkMode ssc m => NEBlocks ssc -> m ()
usRollbackBlocks _ = pass

-- | Verify whether sequence of blocks can be applied to US part of
-- current ChainState DB.  This function doesn't make pure checks,
-- they are assumed to be done earlier.
usVerifyBlocks :: WorkMode ssc m => NEBlocks ssc -> m VerificationRes
usVerifyBlocks _ = pure mempty
