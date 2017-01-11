module Pos.Security.Types
       ( AttackType (..)
       , AttackTarget (..)
       ) where

import           Universum

import           Pos.Types.Address (StakeholderId)
import           Pos.Util.TimeWarp (NetworkAddress)

data AttackType = AttackNoBlocks
                | AttackNoCommitments
                deriving (Eq, Show)

data AttackTarget = NetworkAddressTarget { attNetworkAddr :: NetworkAddress }
                  | PubKeyAddressTarget { attPkAddr :: StakeholderId }
                  deriving (Eq, Show)
