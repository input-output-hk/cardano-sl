module Pos.Security.CLI
       ( AttackType (..)
       , AttackTarget (..)
       , NodeAttackedError (..)
       ) where

import           Universum

import           Pos.Core.Types (StakeholderId)
import           Pos.Util.TimeWarp (NetworkAddress)

data AttackType
    = AttackNoBlocks
    | AttackNoCommitments
    deriving (Eq, Show)

data AttackTarget
    = NetworkAddressTarget { attNetworkAddr :: NetworkAddress}
    | PubKeyAddressTarget { attPkAddr :: StakeholderId}
    deriving (Eq, Show)

-- TODO Move to Pos.Secirity.Types

data NodeAttackedError = AttackNoBlocksTriggered
    deriving Show

instance Exception NodeAttackedError
