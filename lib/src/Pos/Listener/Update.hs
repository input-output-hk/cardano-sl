{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- TODO rename the module / move defintions / whatever.
-- It's not about the network at all.

module Pos.Listener.Update
       ( UpdateMode
       , handleProposal
       , handleVote
       ) where

import           Universum

import           Formatting (build, sformat, (%))
import           System.Wlog (WithLogger, logNotice, logWarning)
import           UnliftIO (MonadUnliftIO)

import           Pos.Core (ProtocolMagic)
import           Pos.Core.Update (UpdateProposal (..), UpdateVote (..))
import           Pos.DB.Class (MonadDB, MonadGState)
import           Pos.Infra.Recovery.Info (MonadRecoveryInfo)
import           Pos.Infra.Reporting (MonadReporting)
import           Pos.Infra.Shutdown.Class (HasShutdownContext)
import           Pos.Infra.Slotting (MonadSlots)
import           Pos.Infra.StateLock (StateLock)
import           Pos.Lrc.Context (HasLrcContext)
import           Pos.Update.Configuration (HasUpdateConfiguration)
import           Pos.Update.Context (UpdateContext)
import           Pos.Update.Logic.Local (processProposal, processVote)
import           Pos.Update.Params (UpdateParams)
import           Pos.Util.Util (HasLens (..))




type UpdateMode ctx m
    = ( WithLogger m
      , MonadIO m
      , MonadUnliftIO m
      , MonadMask m
      , MonadGState m
      , MonadDB m
      , MonadReader ctx m
      , HasLrcContext ctx
      , HasLens UpdateContext ctx UpdateContext
      , HasLens UpdateParams ctx UpdateParams
      , HasLens StateLock ctx StateLock
      , HasShutdownContext ctx
      , HasUpdateConfiguration
      , MonadReporting m
      , MonadRecoveryInfo m
      , MonadSlots ctx m
      )

handleProposal
    :: forall ctx m . UpdateMode ctx m
    => ProtocolMagic
    -> (UpdateProposal, [UpdateVote])
    -> m Bool
handleProposal pm (proposal, votes) = do
    res <- processProposal pm proposal
    logProp proposal res
    let processed = isRight res
    processed <$ when processed (mapM_ processVoteLog votes)
  where
    processVoteLog :: UpdateVote -> m ()
    processVoteLog vote = processVote pm vote >>= logVote vote
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
    :: UpdateMode ctx m
    => ProtocolMagic
    -> UpdateVote
    -> m Bool
handleVote pm uv = do
    res <- processVote pm uv
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
