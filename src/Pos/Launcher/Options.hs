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
    error "Conflicting distribution options were enabled. \
          \Choose one at most or nothing."

stakesDistr
    :: DistrOption
    -> DistrOption
    -> Maybe (Int, Int, Integer, Double)
    -> Bool
    -> StakeDistribution
stakesDistr Nothing Nothing Nothing False = def
stakesDistr (Just (nodes, coins)) Nothing Nothing False =
    FlatStakes (fromIntegral nodes) (mkCoin (fromIntegral coins))
stakesDistr Nothing (Just (nodes, coins)) Nothing False =
    BitcoinStakes (fromIntegral nodes) (mkCoin (fromIntegral coins))
stakesDistr Nothing Nothing (Just (richs, poors, coins, richShare)) False =
    checkConsistency $ RichPoorStakes {..}
  where
    sdRichmen = fromIntegral richs
    sdPoor = fromIntegral poors

    totalRichStake = round $ richShare * fromIntegral coins
    totalPoorStake = coins - totalRichStake
    richStake = totalRichStake `div` fromIntegral richs
    poorStake = totalPoorStake `div` fromIntegral poors
    sdRichStake = mkCoin $ fromIntegral richStake
    sdPoorStake = mkCoin $ fromIntegral poorStake

    checkConsistency =
        if poorStake <= 0 || richStake <= 0
        then error "Impossible to make RichPoorStakes with given parameters."
        else identity
stakesDistr Nothing Nothing Nothing True = ExponentialStakes
stakesDistr _ _ _ _ = panicConflicting
