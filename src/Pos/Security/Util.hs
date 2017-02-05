module Pos.Security.Util
       ( shouldIgnoreAddress
       , shouldIgnorePkAddress
       ) where

import           Universum

import           Pos.Context       (NodeContext, ncNodeParams, npAttackTargets,
                                    npAttackTypes)
import           Pos.Security.CLI  (AttackTarget (..), AttackType (..))
import           Pos.Types         (StakeholderId)
import           Pos.Util.TimeWarp (NetworkAddress)

shouldIgnoreAddress :: NodeContext ssc -> NetworkAddress -> Bool
shouldIgnoreAddress cont addr = and [
    elem AttackNoBlocks $ npAttackTypes $ ncNodeParams cont,
    elem (NetworkAddressTarget addr) $ npAttackTargets $ ncNodeParams cont ]

shouldIgnorePkAddress :: NodeContext ssc -> StakeholderId -> Bool
shouldIgnorePkAddress cont addr = and [
    elem AttackNoCommitments $ npAttackTypes $ ncNodeParams cont,
    elem (PubKeyAddressTarget addr) $ npAttackTargets $ ncNodeParams cont ]
