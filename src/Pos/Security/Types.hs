module Pos.Security.Types
       ( AttackType (..)
       , AttackTarget (..)
       ) where

import           Universum

import           Control.TimeWarp.Rpc (NetworkAddress)
import           Pos.Crypto           (PublicKey)
import           Pos.Types.Address    (AddressHash)

data AttackType = AttackNoBlocks
                | AttackNoCommitments
                deriving (Eq, Show)

data AttackTarget = NetworkAddressTarget { attNetworkAddr :: NetworkAddress }
                  | PubKeyAddressTarget { attPkAddr :: AddressHash PublicKey }
                  deriving (Eq, Show)
