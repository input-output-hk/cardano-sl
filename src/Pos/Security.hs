{-# LANGUAGE FlexibleContexts #-}

module Pos.Security
       ( module Pos.Security.Workers
       , module Pos.Security.Types

       , shouldIgnoreAddress
       ) where

import           Control.TimeWarp.Rpc (NetworkAddress)
import           Universum

import           Pos.Context          (NodeContext, ncAttackTypes, ncAttackTargets)
import           Pos.Security.Types
import           Pos.Security.Workers

shouldIgnoreAddress :: NodeContext ssc -> NetworkAddress -> Bool
shouldIgnoreAddress cont addr = and [ elem AttackNoBlocks $ ncAttackTypes cont
                                    , elem addr $ map attNetworkAddr $ ncAttackTargets cont ]
