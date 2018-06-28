module InputSelection.TxStats (
    TxStats(..)
  , deriveTxStats
    -- * Convenience re-exports
  , Fixed
  , E2
  ) where

import           Universum

import           Data.Fixed (E2, Fixed)

import           Cardano.Wallet.Kernel.CoinSelection.Generic

import           Util.Histogram (BinSize (..), Histogram)
import qualified Util.Histogram as Histogram
import           Util.MultiSet (MultiSet)
import qualified Util.MultiSet as MultiSet

{-------------------------------------------------------------------------------
  Transaction stastistics
-------------------------------------------------------------------------------}

-- | Transaction statistics
--
-- Transaction statistics are used for policy evaluation. For "real" input
-- selection policies we don't necessarily need to return this information,
-- although it may be beneficial to do so even there -- it may be useful
-- to monitor these statistics and learn something about the wallet as it
-- operates in reality.
data TxStats = TxStats {
      -- | Number of inputs
      --
      -- This is a histogram because although a single transaction only has
      -- a single value for its number of inputs, recording this as a histogram
      -- allows us to combine the statistics of many transactions.
      txStatsNumInputs :: !Histogram

      -- | Change/payment ratios
    , txStatsRatios    :: !(MultiSet (Fixed E2))
    }

instance Semigroup TxStats where
   a <> b = TxStats {
            txStatsNumInputs = mappendUsing Histogram.add  txStatsNumInputs
          , txStatsRatios    = mappendUsing MultiSet.union txStatsRatios
          }
    where
      mappendUsing :: (a -> a -> a) -> (TxStats -> a) -> a
      mappendUsing op f = f a `op` f b

instance Monoid TxStats where
  mempty = TxStats {
        txStatsNumInputs = Histogram.empty (BinSize 1)
      , txStatsRatios    = MultiSet.empty
      }
  mappend = (<>)

{-------------------------------------------------------------------------------
  Partial transaction statistics
-------------------------------------------------------------------------------}

-- | Partial transaciton statistics
--
-- Partial transactions statistics are useful when constructing a transaciton
-- piece by piece.
data PartialTxStats = PartialTxStats {
      -- | Number of inputs
      --
      -- Unlike for 'TxStats', this is not a histogram. Suppose we have two
      -- 'PartialTxStats' with 'ptxStatsNumInputs' equal to @n@ and @m@.
      -- Then the final histogram should have a single bin at @n + m@ with
      -- count 1. This is rather different from having two transactions with
      -- @n@ inputs and @m@ outputs; this would result in a histogram with
      -- /two/ bins at @n@ and @m@ both with count 1, or, if @n == m@, a
      -- single bin at @n@ with count 2.
      ptxStatsNumInputs :: !Int

      -- | Change/payment ratios
    , ptxStatsRatios    :: !(MultiSet (Fixed E2))
    }

instance Semigroup PartialTxStats where
  a <> b = PartialTxStats {
             ptxStatsNumInputs = mappendUsing (+)            ptxStatsNumInputs
           , ptxStatsRatios    = mappendUsing MultiSet.union ptxStatsRatios
           }
    where
      mappendUsing :: (a -> a -> a) -> (PartialTxStats -> a) -> a
      mappendUsing op f = f a `op` f b

instance Monoid PartialTxStats where
  mempty = PartialTxStats {
        ptxStatsNumInputs = 0
      , ptxStatsRatios    = MultiSet.empty
      }
  mappend = (<>)

-- | Construct transaciton statistics from partial statistics
fromPartialTxStats :: PartialTxStats -> TxStats
fromPartialTxStats PartialTxStats{..} = TxStats{
      txStatsNumInputs = Histogram.fromList (BinSize 1) [(ptxStatsNumInputs, 1)]
    , txStatsRatios    = ptxStatsRatios
    }

{-------------------------------------------------------------------------------
  Derivation
-------------------------------------------------------------------------------}

-- | Derive transaction statistics from coin selection result
--
-- We take as argument a function @f@ such that @f value change@ computes
-- the change:value ratio
deriveTxStats :: forall dom. CoinSelDom dom => [CoinSelResult dom] -> TxStats
deriveTxStats = fromPartialTxStats . mconcat . map go
  where
    go :: CoinSelResult dom -> PartialTxStats
    go cs = PartialTxStats {
          ptxStatsNumInputs = fromIntegral $ sizeToWord (coinSelInputSize cs)
        , ptxStatsRatios    = MultiSet.fromList $
                                map (computeRatio (outVal (coinSelOutput cs)))
                                    (coinSelChange cs)
        }

    computeRatio :: Value dom -> Value dom -> Fixed E2
    computeRatio out change = realToFrac (valueRatio change out)
