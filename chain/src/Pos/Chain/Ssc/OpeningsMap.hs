module Pos.Chain.Ssc.OpeningsMap
       ( OpeningsMap
       ) where

import           Universum

import           Pos.Core.Common (StakeholderId)

import           Pos.Chain.Ssc.Opening

type OpeningsMap = HashMap StakeholderId Opening
