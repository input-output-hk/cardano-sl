module Pos.Security.Util
       ( shouldIgnoreAddress
       , shouldIgnorePkAddress
       ) where

import           Universum

import           Pos.Context       (NodeParams, npAttackTargets, npAttackTypes)
import           Pos.Security.CLI  (AttackTarget (..), AttackType (..))
import           Pos.Types         (StakeholderId)
import           Pos.Util.TimeWarp (NetworkAddress)

shouldIgnoreAddress :: NodeParams -> NetworkAddress -> Bool
shouldIgnoreAddress np addr = and [
    elem AttackNoBlocks $ npAttackTypes np,
    elem (NetworkAddressTarget addr) $ npAttackTargets np ]

shouldIgnorePkAddress :: NodeParams -> StakeholderId -> Bool
shouldIgnorePkAddress np addr = and [
    elem AttackNoCommitments $ npAttackTypes np,
    elem (PubKeyAddressTarget addr) $ npAttackTargets np ]
