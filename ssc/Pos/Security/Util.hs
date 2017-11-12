module Pos.Security.Util
       ( shouldIgnoreAddress
       , shouldIgnorePkAddress
       ) where

import           Universum

import           Ether.Internal (HasLens (..))

import           Pos.Core (StakeholderId)
import           Pos.Security.Params (AttackTarget (..), AttackType (..), SecurityParams (..))
import           Pos.Util.TimeWarp (NetworkAddress)

shouldIgnoreAddress
    :: (MonadReader ctx m, HasLens SecurityParams ctx SecurityParams)
    => NetworkAddress -> m Bool
shouldIgnoreAddress addr = do
    SecurityParams{..} <- view (lensOf @SecurityParams)
    return $ and [
        AttackNoBlocks `elem` spAttackTypes,
        NetworkAddressTarget addr `elem` spAttackTargets ]

shouldIgnorePkAddress
    :: (MonadReader ctx m, HasLens SecurityParams ctx SecurityParams)
    => StakeholderId -> m Bool
shouldIgnorePkAddress addr = do
    SecurityParams{..} <- view (lensOf @SecurityParams)
    return $ and [
        AttackNoCommitments `elem` spAttackTypes,
        PubKeyAddressTarget addr `elem` spAttackTargets ]
