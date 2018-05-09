{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- TODO rename the module / move defintions / whatever.
-- It's not about the network at all.

module Pos.Update.Network.Listeners
       ( handleProposal
       , handleVote
       ) where

import           Universum

import           Formatting (build, sformat, (%))
import           System.Wlog (WithLogger, logNotice, logWarning)

import           Pos.Core.Update (UpdateProposal (..), UpdateVote (..))
import           Pos.Core (HasGeneratedSecrets, HasProtocolConstants, HasGenesisBlockVersionData)
import           Pos.Update.Logic.Local (processProposal, processVote)
import           Pos.Update.Mode (UpdateMode)

handleProposal
    :: forall ctx m .
       (UpdateMode ctx m, HasGenesisBlockVersionData, HasProtocolConstants)
    => (UpdateProposal, [UpdateVote])
    -> m Bool
handleProposal (proposal, votes) = do
    res <- processProposal proposal
    logProp proposal res
    let processed = isRight res
    processed <$ when processed (mapM_ processVoteLog votes)
  where
    processVoteLog :: UpdateVote -> m ()
    processVoteLog vote = processVote vote >>= logVote vote
    logVote vote (Left cause) =
        logWarning $ sformat ("Proposal is accepted but vote "%build%
                              " is rejected, the reason is: "%build)
                     vote cause
    logVote vote (Right _) = logVoteAccepted vote

    logProp prop (Left cause) =
        logWarning $ sformat ("Processing of proposal "%build%
                              " failed, the reason is: "%build)
              prop cause
    -- Update proposals are accepted rarely (at least before Shelley),
    -- so it deserves 'Notice' severity.
    logProp prop (Right _) =
        logNotice $ sformat ("Processing of proposal "%build%" is successful")
              prop

----------------------------------------------------------------------------
-- UpdateVote
----------------------------------------------------------------------------

handleVote
    :: forall ctx m .
       (UpdateMode ctx m, HasGeneratedSecrets, HasProtocolConstants, HasGenesisBlockVersionData)
    => UpdateVote
    -> m Bool
handleVote uv = do
    res <- processVote uv
    logProcess uv res
    pure $ isRight res
  where
    logProcess vote (Left cause) =
        logWarning $ sformat ("Processing of vote "%build%
                              "failed, the reason is: "%build)
                     vote cause
    logProcess vote (Right _) = logVoteAccepted vote

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- Update votes are accepted rarely (at least before Shelley), so
-- it deserves 'Notice' severity.
logVoteAccepted :: WithLogger m => UpdateVote -> m ()
logVoteAccepted =
    logNotice . sformat ("Processing of vote "%build%"is successfull")
