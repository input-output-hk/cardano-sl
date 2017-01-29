-- | Basic functionality from Toss.

module Pos.Ssc.GodTossing.Toss.Base
       ( checkOpeningMatchesCommitment
       ) where

import qualified Data.HashMap.Strict     as HM
import           Universum

import           Pos.Ssc.GodTossing.Core (CommitmentsMap (getCommitmentsMap), Opening,
                                          verifyOpening)
import           Pos.Types               (StakeholderId)

-- CHECK: @checkOpeningMatchesCommitment
-- | Check that the secret revealed in the opening matches the secret proof
-- in the commitment
checkOpeningMatchesCommitment
    :: CommitmentsMap -> (StakeholderId, Opening) -> Bool
checkOpeningMatchesCommitment (getCommitmentsMap -> globalCommitments) (addr, opening) =
      case HM.lookup addr globalCommitments of
        Nothing           -> False
        Just (_, comm, _) -> verifyOpening comm opening
