module Pos.Core.Common.Stakes
       ( StakesMap
       , StakesList
       ) where

import           Universum

import           Pos.Core.Common.Coin
import           Pos.Core.Common.StakeholderId

-- | A mapping between stakeholders and they stakes.
type StakesMap = HashMap StakeholderId Coin

-- | Stakeholders and their stakes.
type StakesList = [(StakeholderId, Coin)]
