{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Rollback logic in Poll.

module Pos.Update.Poll.Logic.Rollback
       ( rollbackUS
       ) where

import qualified Data.HashMap.Strict   as HM
import           Universum

import           Pos.Types             (SoftwareVersion (..))
import           Pos.Types.Version     (ApplicationName, BlockVersion, NumSoftwareVersion)
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
    -- Rollback last adopted
    whenJust unLastAdoptedBV setLastAdoptedBV
    -- Rollback block version
    mapM_ setOrDelBV $ HM.toList unChangedBV
  where
    setOrDelLastConfirmedSV :: (ApplicationName, PrevValue NumSoftwareVersion) -> m ()
    setOrDelLastConfirmedSV (svAppName, PrevValue svNumber) =
        setLastConfirmedSV SoftwareVersion {..}
    setOrDelLastConfirmedSV (appName, NoExist) = delConfirmedSV appName

    setOrDelProposal :: (UpId, PrevValue ProposalState) -> m ()
    setOrDelProposal (upid, NoExist)   = deactivateProposal upid
    setOrDelProposal (_, PrevValue ps) = addActiveProposal ps

    setOrDelBV :: (BlockVersion, PrevValue BlockVersionState) -> m ()
    setOrDelBV (bv, NoExist)       = delBVState bv
    setOrDelBV (bv, PrevValue bvs) = putBVState bv bvs
