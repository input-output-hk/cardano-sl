module Pos.Security.Util
       ( shouldIgnoreAddress
       , shouldIgnorePkAddress
       ) where

import           Universum

import           Pos.Context       (NodeContext, ncAttackTargets, ncAttackTypes)
import           Pos.Security.CLI  (AttackTarget (..), AttackType (..))
import           Pos.Types         (StakeholderId)
import           Pos.Util.TimeWarp (NetworkAddress)

shouldIgnoreAddress :: NodeContext ssc -> NetworkAddress -> Bool
shouldIgnoreAddress cont addr = and [
    elem AttackNoBlocks $ ncAttackTypes cont,
    elem (NetworkAddressTarget addr) $ ncAttackTargets cont ]

shouldIgnorePkAddress :: NodeContext ssc -> StakeholderId -> Bool
shouldIgnorePkAddress cont addr = and [
    elem AttackNoCommitments $ ncAttackTypes cont,
    elem (PubKeyAddressTarget addr) $ ncAttackTargets cont ]
