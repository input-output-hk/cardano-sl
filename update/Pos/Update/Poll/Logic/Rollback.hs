{-# LANGUAGE ScopedTypeVariables #-}

-- | Rollback logic in Poll.

module Pos.Update.Poll.Logic.Rollback
       ( rollbackUS
       ) where

import qualified Data.HashMap.Strict   as HM
import           Universum

import           Pos.Core              (ApplicationName, BlockVersion, NumSoftwareVersion,
                                        SoftwareVersion (..))
import           Pos.Update.Core       (UpId)
import           Pos.Update.Poll.Class (MonadPoll (..))
import           Pos.Update.Poll.Types (BlockVersionState, PrevValue (..),
                                        ProposalState (..), USUndo (..))

-- | Rollback application of UpdatePayload in MonadPoll using payload
-- itself and undo data.
rollbackUS
    :: forall m . MonadPoll m
    => USUndo -> m ()
rollbackUS USUndo{..} = do
    -- Rollback last confirmed
    mapM_ setOrDelLastConfirmedSV $ HM.toList unChangedSV
    -- Rollback proposals
    mapM_ setOrDelProposal $ HM.toList unChangedProps
    -- Rollback block version. It's important to do it before next step,
    -- because setAdoptedBV takes state from Poll, not as argument.
    mapM_ setOrDelBV $ HM.toList unChangedBV
    -- Rollback last adopted
    whenJust unLastAdoptedBV setAdoptedBV
    whenJust unPrevProposers setEpochProposers
  where
    setOrDelLastConfirmedSV :: (ApplicationName, PrevValue NumSoftwareVersion) -> m ()
    setOrDelLastConfirmedSV (svAppName, PrevValue svNumber) =
        setLastConfirmedSV SoftwareVersion {..}
    setOrDelLastConfirmedSV (appName, NoExist) = delConfirmedSV appName

    setOrDelProposal :: (UpId, PrevValue ProposalState) -> m ()
    setOrDelProposal (upid, NoExist)   = deactivateProposal upid
    setOrDelProposal (_, PrevValue ps) = insertActiveProposal ps

    setOrDelBV :: (BlockVersion, PrevValue BlockVersionState) -> m ()
    setOrDelBV (bv, NoExist)       = delBVState bv
    setOrDelBV (bv, PrevValue bvs) = putBVState bv bvs
