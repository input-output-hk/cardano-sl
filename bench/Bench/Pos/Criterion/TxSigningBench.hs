module Bench.Pos.Criterion.TxSigningBench
       ( runBenchmark
       ) where

import           Criterion.Main     (Benchmark, bench, defaultConfig, defaultMainWith,
                                     env, whnf)
import           Criterion.Types    (Config (..))
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Data.Maybe
import           Test.QuickCheck    (generate)
import           Universum

import           Pos.Crypto         (SecretKey, hash, sign)
import           Pos.Txp            (TxDistribution (..), TxId, TxOut, TxSig)
import           Pos.Util           (arbitraryUnsafe)

signTx :: (SecretKey, TxId, Word32, NonEmpty TxOut) -> TxSig
signTx (sk, thash, tidx, touts) = sign sk (thash, tidx, hash touts, hash distr)
  where
    distr = TxDistribution ([] :| replicate (length touts - 1) [])

txSignBench :: Benchmark
txSignBench = env genArgs $ bench "Transactions signing" . whnf signTx
  where genArgs = generate $ (,,,)
                  <$> arbitraryUnsafe
                  <*> arbitraryUnsafe
                  <*> arbitraryUnsafe
                  <*> arbitraryUnsafe

txSignConfig :: Config
txSignConfig = defaultConfig
    { reportFile = Just "txSigning.html"
    }

runBenchmark :: IO ()
runBenchmark = defaultMainWith txSignConfig [txSignBench]
