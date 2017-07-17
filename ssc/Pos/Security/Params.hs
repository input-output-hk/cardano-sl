module Pos.Security.Params
       ( SecurityParams(..)
       , AttackType(..)
       , AttackTarget(..)
       , NodeAttackedError(..)
       ) where

import           Universum

import           Pos.Core.Types    (StakeholderId)
import           Pos.Util.TimeWarp (NetworkAddress)

data SecurityParams = SecurityParams
    { -- | List of attack types used by malicious emulation
      spAttackTypes   :: ![AttackType]
      -- | List of targets to attack by malicious emulation
    , spAttackTargets :: ![AttackTarget]
    }
    deriving (Show)

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
