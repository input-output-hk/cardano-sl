{-# LANGUAGE FlexibleContexts #-}

module Pos.Security.Util
       ( shouldIgnoreAddress
       , shouldIgnorePkAddress
       ) where

import           Control.TimeWarp.Rpc (NetworkAddress)
import           Universum

import           Pos.Context          (NodeContext, ncAttackTargets, ncAttackTypes)
import           Pos.Crypto           (PublicKey)
import           Pos.Security.Types   (AttackTarget (..), AttackType (..))
import           Pos.Types.Address    (AddressHash)

shouldIgnoreAddress :: NodeContext ssc -> NetworkAddress -> Bool
shouldIgnoreAddress cont addr = and [
    elem AttackNoBlocks $ ncAttackTypes cont,
    elem (NetworkAddressTarget addr) $ ncAttackTargets cont ]

shouldIgnorePkAddress :: NodeContext ssc -> AddressHash PublicKey -> Bool
shouldIgnorePkAddress cont addr = and [
    elem AttackNoCommitments $ ncAttackTypes cont,
    elem (PubKeyAddressTarget addr) $ ncAttackTargets cont ]
