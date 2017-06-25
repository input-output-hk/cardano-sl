module Pos.Security.Util
       ( shouldIgnoreAddress
       , shouldIgnorePkAddress
       ) where

import           Universum

import           EtherCompat

import           Pos.Core            (StakeholderId)
import           Pos.Security.Params (AttackTarget (..), AttackType (..),
                                      SecurityParams (..))
import           Pos.Util.TimeWarp   (NetworkAddress)

shouldIgnoreAddress
    :: MonadCtx ctx SecurityParams SecurityParams m
    => NetworkAddress -> m Bool
shouldIgnoreAddress addr = do
    SecurityParams{..} <- askCtx @SecurityParams
    return $ and [
        AttackNoBlocks `elem` spAttackTypes,
        NetworkAddressTarget addr `elem` spAttackTargets ]

shouldIgnorePkAddress
    :: MonadCtx ctx SecurityParams SecurityParams m
    => StakeholderId -> m Bool
shouldIgnorePkAddress addr = do
    SecurityParams{..} <- askCtx @SecurityParams
    return $ and [
        AttackNoCommitments `elem` spAttackTypes,
        PubKeyAddressTarget addr `elem` spAttackTargets ]
