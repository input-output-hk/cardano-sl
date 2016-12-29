module Pos.Security.Types
       ( AttackType (..)
       , AttackTarget (..)
       ) where

import           Universum

import           Control.TimeWarp.Rpc (NetworkAddress)
import           Pos.Types.Address    (NodeId)

data AttackType = AttackNoBlocks
                | AttackNoCommitments
                deriving (Eq, Show)

data AttackTarget = NetworkAddressTarget { attNetworkAddr :: NetworkAddress }
                  | PubKeyAddressTarget { attPkAddr :: NodeId }
                  deriving (Eq, Show)
