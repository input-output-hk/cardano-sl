
-- | Functions which operate on MonadPoll[Read]. Business logic of
-- Update System.

module Pos.Update.Poll.Logic
       ( verifyAndApplyUSPayload
       , rollbackUS
       , normalizePoll
       , filterProposalsByThd

       -- * Base
       , canCreateBlockBV
       , verifyBlockSize

       -- * Softfork resolution
       , processGenesisBlock
       , recordBlockIssuance
       ) where

import           Pos.Update.Poll.Logic.Apply
import           Pos.Update.Poll.Logic.Base
import           Pos.Update.Poll.Logic.Normalize
import           Pos.Update.Poll.Logic.Rollback
import           Pos.Update.Poll.Logic.Softfork
