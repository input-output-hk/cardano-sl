
-- | Rollback logic in Poll.

module Pos.Update.Poll.Logic.Rollback
       ( rollbackUS
       ) where

import qualified Data.HashMap.Strict as HM
import           Universum

import           Pos.Core (ApplicationName, BlockVersion, NumSoftwareVersion, SoftwareVersion (..))
import           Pos.Core.Update (UpId)
import           Pos.Update.Poll.Class (MonadPoll (..))
import           Pos.Update.Poll.Types (BlockVersionState, ConfirmedProposalState, PrevValue (..),
                                        ProposalState (..), USUndo (..))

-- | Rollback application of UpdatePayload in MonadPoll using payload
-- itself and undo data.
rollbackUS
    :: forall m . MonadPoll m
    => USUndo -> m ()
-- Note: here we use explicit pattern-matching which forces us to
-- enumerate all fields to avoid situation when we add something new
-- to 'USUndo' and forget to consider it in 'rollbackUS'.
rollbackUS (USUndo changedBlockVersions
                   lastAdoptedBV
                   changedActiveProposals
                   changedConfirmedSVs
                   changedConfirmedProposals
                   prevProposers
                   slottingData) = do
    -- Rollback last confirmed proposals (with their states) for applications.
    mapM_ setOrDelLastConfirmedSV $ HM.toList changedConfirmedSVs
    -- Rollback confirmed proposals states for software versions.
    mapM_ setOrDelLastConfirmedPropState $ HM.toList changedConfirmedProposals
    -- Rollback proposals
    mapM_ setOrDelProposal $ HM.toList changedActiveProposals
    -- Rollback block version. It's important to do it before next step,
    -- because setAdoptedBV takes state from Poll, not as argument.
    mapM_ setOrDelBV $ HM.toList changedBlockVersions
    -- Rollback last adopted
    whenJust lastAdoptedBV setAdoptedBV
    whenJust prevProposers setEpochProposers
    whenJust slottingData setSlottingData
  where
    setOrDelLastConfirmedSV ::
           (ApplicationName, PrevValue NumSoftwareVersion) -> m ()
    setOrDelLastConfirmedSV (svAppName, PrevValue svNumber) =
        setLastConfirmedSV SoftwareVersion {..}
    setOrDelLastConfirmedSV (appName, NoExist) = delConfirmedSV appName

    setOrDelLastConfirmedPropState ::
           (SoftwareVersion, PrevValue ConfirmedProposalState) -> m ()
    setOrDelLastConfirmedPropState (_, PrevValue propState) =
        addConfirmedProposal propState
    setOrDelLastConfirmedPropState (sw, NoExist) = delConfirmedProposal sw

    setOrDelProposal :: (UpId, PrevValue ProposalState) -> m ()
    setOrDelProposal (upid, NoExist)   = deactivateProposal upid
    setOrDelProposal (_, PrevValue ps) = insertActiveProposal ps

    setOrDelBV :: (BlockVersion, PrevValue BlockVersionState) -> m ()
    setOrDelBV (bv, NoExist)       = delBVState bv
    setOrDelBV (bv, PrevValue bvs) = putBVState bv bvs
