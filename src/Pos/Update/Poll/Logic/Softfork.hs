-- | Softfork resolution logic.

module Pos.Update.Poll.Logic.Softfork
       ( recordBlockIssuance
       , processGenesisBlock
       ) where

import           Universum

import           Pos.Types             (BlockVersion, EpochIndex, StakeholderId)
import           Pos.Update.Poll.Class (MonadPoll (..))

-- | Record the fact that block with given version has been issued by
-- stakeholder with given id.
recordBlockIssuance
    :: MonadPoll m
    => StakeholderId -> BlockVersion -> m ()
recordBlockIssuance _ _ = pass

-- | Process creation of genesis block for given epoch.
processGenesisBlock
    :: MonadPoll m
    => EpochIndex -> m ()
processGenesisBlock _ = pass
