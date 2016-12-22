{-# LANGUAGE FlexibleContexts #-}

module Pos.Security.Util
       ( shouldIgnoreAddress
       , shouldIgnorePkAddress
       ) where

import           Control.TimeWarp.Rpc (NetworkAddress)
import           Universum

import           Pos.Context          (NodeContext, ncAttackTargets, ncAttackTypes)
import           Pos.Security.Types   (AttackTarget (..), AttackType (..))
import           Pos.Types.Address    (Address (..), AddressDestination (..))

shouldIgnoreAddress :: NodeContext ssc -> NetworkAddress -> Bool
shouldIgnoreAddress cont addr = and [
    elem AttackNoBlocks $ ncAttackTypes cont,
    elem (NetworkAddressTarget addr) $ ncAttackTargets cont ]

shouldIgnorePkAddress :: NodeContext ssc -> Address -> Bool
shouldIgnorePkAddress cont addr = case addrDestination addr of
    PubKeyDestination{..} -> and [
        elem AttackNoCommitments $ ncAttackTypes cont,
        elem (PubKeyAddressTarget addrDestKeyHash) $ ncAttackTargets cont ]
    _ -> False
