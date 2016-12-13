module Bench.Pos.Criterion.TxVerifyingBench
       ( runBenchmark
       ) where

import           Control.Lens         (view, _1, _4)
import           Criterion.Main       (Benchmark, bench, defaultConfig, defaultMainWith,
                                       env, whnf)
import           Criterion.Types       (Config (..))
import           Data.Maybe
import qualified Data.Vector          as V (fromList)
import           Test.QuickCheck      (arbitrary, generate)
import           Universum
import           Serokell.Util.Verify (VerificationRes)

import           Pos.Crypto           (withHash)
import           Pos.Types            (GoodTx (..), Tx (..), TxWitness, Utxo,
                                       applyTxToUtxo, verifyTxUtxo)

verifyTx :: (Utxo, Tx, TxWitness) -> VerificationRes
verifyTx (u, tx, tw) = verifyTxUtxo u (tx, tw)

txVerifyBench :: Benchmark
txVerifyBench = env genArgs $ bench "Transaction verifying" . whnf verifyTx
  where
    genArgs = generate $ do
        ls <- getGoodTx <$> arbitrary
        let txs = fmap (view _1) ls
            witness = V.fromList $ fmap (view _4) ls
            newTx = uncurry Tx $ unzip $ fmap (\(_, tIs, tOs, _) -> (tIs, tOs)) ls
            utxo = foldr applyTxToUtxo mempty (fmap withHash txs)
        return (utxo, newTx, witness)

txVerifyConfig :: Config
txVerifyConfig = defaultConfig
    { reportFile = Just "txVerifying.html"
    }

runBenchmark :: IO ()
runBenchmark = defaultMainWith txVerifyConfig [txVerifyBench]
