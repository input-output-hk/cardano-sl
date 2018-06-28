module Pos.Security.Util
       ( shouldIgnoreAddress
       , shouldIgnorePkAddress
       ) where

import           Universum


import           Pos.Core (StakeholderId)
import           Pos.Infra.Util.TimeWarp (NetworkAddress)
import           Pos.Security.Params (AttackTarget (..), AttackType (..),
                     SecurityParams (..))
import           Pos.Util.Util (HasLens (..))

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
