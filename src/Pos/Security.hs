{-# LANGUAGE FlexibleContexts #-}

module Pos.Security
       ( module Pos.Security.Workers
       , module Pos.Security.Types

       , shouldIgnoreAddress
       ) where

import           Control.TimeWarp.Rpc (NetworkAddress)
import           Universum

import           Pos.Context          (getNodeContext, ncAttackTypes, ncAttackTargets)
import           Pos.Security.Types
import           Pos.Security.Workers
import           Pos.WorkMode         (WorkMode)

shouldIgnoreAddress :: WorkMode scc m => NetworkAddress -> m Bool
shouldIgnoreAddress addr = and <$> sequence
                           [ elem AttackNoBlocks <$> ncAttackTypes <$> getNodeContext
                           , elem addr <$> map attNetworkAddr <$> ncAttackTargets <$> getNodeContext ]
