{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Rollback logic in Poll.

module Pos.Update.Poll.Logic.Rollback
       ( rollbackUSPayload
       ) where

import qualified Data.HashMap.Strict   as HM
import           Universum

import           Pos.Types             (ChainDifficulty, SoftwareVersion (..))
import           Pos.Types.Version     (ApplicationName, NumSoftwareVersion)
import           Pos.Update.Core       (UpId, UpdatePayload (..))
import           Pos.Update.Poll.Class (MonadPoll (..))
import           Pos.Update.Poll.Types (PrevValue (..), ProposalState (..), USUndo (..))

-- | Rollback application of UpdatePayload in MonadPoll using payload
-- itself and undo data.
rollbackUSPayload
    :: forall m . MonadPoll m
    => ChainDifficulty -> UpdatePayload -> USUndo -> m ()
rollbackUSPayload _ UpdatePayload{..} USUndo{..} = do
    -- Rollback last confirmed
    mapM_ setOrDelLastConfirmedSV (HM.toList unChangedSV)
    -- Rollback proposals
    mapM_ setOrDelProposal (HM.toList unChangedProps)
    -- Rollback protocol version
    whenJust unLastAdoptedBV setLastAdoptedBV
    -- Rollback script
    whenJust unCreatedNewBSFor delBVState
  where
    setOrDelLastConfirmedSV :: (ApplicationName, PrevValue NumSoftwareVersion) -> m ()
    setOrDelLastConfirmedSV (svAppName, PrevValue svNumber) =
        setLastConfirmedSV SoftwareVersion {..}
    setOrDelLastConfirmedSV (appName, NoExist) = delConfirmedSV appName

    setOrDelProposal :: (UpId, PrevValue ProposalState) -> m ()
    setOrDelProposal (upid, NoExist)   = deactivateProposal upid
    setOrDelProposal (_, PrevValue ps) = addActiveProposal ps
