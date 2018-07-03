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

import           Pos.Core (ProtocolMagic)
import           Pos.Core.Update (UpdateProposal (..), UpdateVote (..))
import           Pos.Update.Logic.Local (processProposal, processVote)
import           Pos.Update.Mode (UpdateMode)
import           Pos.Util.Trace (natTrace)
import           Pos.Util.Trace.Named (TraceNamed, logNotice, logWarning)

handleProposal
    :: forall ctx m . UpdateMode ctx m
    => TraceNamed IO
    -> ProtocolMagic
    -> (UpdateProposal, [UpdateVote])
    -> m Bool
handleProposal logTrace pm (proposal, votes) = do
    res <- processProposal logTrace pm proposal
    logProp proposal res
    let processed = isRight res
    processed <$ when processed (mapM_ processVoteLog votes)
  where
    logTrace' = natTrace liftIO logTrace
    processVoteLog :: UpdateVote -> m ()
    processVoteLog vote = processVote logTrace pm vote >>= logVote vote
    logVote vote (Left cause) =
        logWarning logTrace' $ sformat ("Proposal is accepted but vote "%build%
                              " is rejected, the reason is: "%build)
                     vote cause
    logVote vote (Right _) = logVoteAccepted logTrace vote

    logProp prop (Left cause) =
        logWarning logTrace' $ sformat ("Processing of proposal "%build%
                              " failed, the reason is: "%build)
              prop cause
    -- Update proposals are accepted rarely (at least before Shelley),
    -- so it deserves 'Notice' severity.
    logProp prop (Right _) =
        logNotice logTrace' $ sformat ("Processing of proposal "%build%" is successful")
              prop

----------------------------------------------------------------------------
-- UpdateVote
----------------------------------------------------------------------------

handleVote
    :: UpdateMode ctx m
    => TraceNamed IO
    -> ProtocolMagic
    -> UpdateVote
    -> m Bool
handleVote logTrace pm uv = do
    res <- processVote logTrace pm uv
    logProcess uv res
    pure $ isRight res
  where
    logTrace' = natTrace liftIO logTrace
    logProcess vote (Left cause) =
        logWarning logTrace' $ sformat ("Processing of vote "%build%
                              "failed, the reason is: "%build)
                     vote cause
    logProcess vote (Right _) = logVoteAccepted logTrace vote

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- Update votes are accepted rarely (at least before Shelley), so
-- it deserves 'Notice' severity.
logVoteAccepted :: MonadIO m => TraceNamed IO -> UpdateVote -> m ()
logVoteAccepted logTrace vote =
    liftIO $ logNotice logTrace $ sformat ("Processing of vote "%build%"is successfull") vote
