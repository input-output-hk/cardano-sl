module Pos.Core.Ssc.OpeningsMap
       ( OpeningsMap
       ) where

import           Universum

import           Pos.Core.Common (StakeholderId)

import           Pos.Core.Ssc.Opening

type OpeningsMap = HashMap StakeholderId Opening
