{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Functions which operate on MonadPoll[Read]. Business logic of
-- Update System.

module Pos.Update.Poll.Logic
       ( verifyAndApplyUSPayload
       , rollbackUSPayload
       , normalizePoll
       , filterProposalsByThd
       , canCreateBlockBV
       ) where

import           Pos.Update.Poll.Logic.Apply
import           Pos.Update.Poll.Logic.Base
import           Pos.Update.Poll.Logic.Normalize
import           Pos.Update.Poll.Logic.Rollback
