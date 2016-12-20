{-# LANGUAGE FlexibleContexts #-}

module Pos.Security.Util
       ( shouldIgnoreAddress
       , shouldIgnorePkAddress
       ) where

import           Control.TimeWarp.Rpc (NetworkAddress)
import           Universum

import           Pos.Context          (NodeContext, ncAttackTargets, ncAttackTypes)
import           Pos.Security.Types   (AttackTarget (..), AttackType (..))
import           Pos.Types.Address    (Address (..))

shouldIgnoreAddress :: NodeContext ssc -> NetworkAddress -> Bool
shouldIgnoreAddress cont addr = and [ elem AttackNoBlocks $ ncAttackTypes cont
                                    , elem (NetworkAddressTarget addr) $ ncAttackTargets cont ]

shouldIgnorePkAddress :: NodeContext ssc -> Address -> Bool
shouldIgnorePkAddress cont PubKeyAddress {..} = and [ elem AttackNoCommitments $ ncAttackTypes cont
                                                    , elem (PubKeyAddressTarget addrKeyHash) $ ncAttackTargets cont ]
shouldIgnorePkAddress _ _ = False
