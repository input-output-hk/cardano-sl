module Pos.Security.Util
       ( shouldIgnoreAddress
       , shouldIgnorePkAddress
       ) where

import           Universum

import qualified Ether

import           Pos.Core            (StakeholderId)
import           Pos.Security.Params (AttackTarget (..), AttackType (..),
                                      SecurityParams (..))
import           Pos.Util.TimeWarp   (NetworkAddress)

shouldIgnoreAddress
    :: Ether.MonadReader' SecurityParams m
    => NetworkAddress -> m Bool
shouldIgnoreAddress addr = do
    SecurityParams{..} <- Ether.ask'
    return $ and [
        AttackNoBlocks `elem` spAttackTypes,
        NetworkAddressTarget addr `elem` spAttackTargets ]

shouldIgnorePkAddress
    :: Ether.MonadReader' SecurityParams m
    => StakeholderId -> m Bool
shouldIgnorePkAddress addr = do
    SecurityParams{..} <- Ether.ask'
    return $ and [
        AttackNoCommitments `elem` spAttackTypes,
        PubKeyAddressTarget addr `elem` spAttackTargets ]
