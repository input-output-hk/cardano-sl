-- | Utils for common operations with CLI params

module Pos.Launcher.Options
       ( stakesDistr
       ) where

import           Data.Default (def)
import           Universum

import           Pos.Genesis  (StakeDistribution (..))
import           Pos.Types    (mkCoin)

type DistrOption = Maybe (Int, Int)

panicConflicting :: panic
panicConflicting =
    panic $ "Conflicting distribution options were enabled. " <>
            "Choose one at most or nothing."

stakesDistr :: DistrOption -> DistrOption -> Bool -> StakeDistribution
stakesDistr Nothing Nothing False = def
stakesDistr (Just (nodes, coins)) Nothing False =
    FlatStakes (fromIntegral nodes) (mkCoin (fromIntegral coins))
stakesDistr Nothing (Just (nodes, coins)) False =
    BitcoinStakes (fromIntegral nodes) (mkCoin (fromIntegral coins))
stakesDistr Nothing Nothing True = ExponentialStakes
stakesDistr _ _ _ = panicConflicting
