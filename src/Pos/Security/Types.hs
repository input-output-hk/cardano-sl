module Pos.Security.Types
       ( AttackType (..)
       , AttackTarget (..)
       ) where

import           Universum

import           Control.TimeWarp.Rpc (NetworkAddress)
import           Pos.Types.Address    (StakeholderId)

data AttackType = AttackNoBlocks
                | AttackNoCommitments
                deriving (Eq, Show)

data AttackTarget = NetworkAddressTarget { attNetworkAddr :: NetworkAddress }
                  | PubKeyAddressTarget { attPkAddr :: StakeholderId }
                  deriving (Eq, Show)
