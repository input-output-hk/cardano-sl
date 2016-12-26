module Bench.Pos.Criterion.TxSigningBench
       ( runBenchmark
       ) where

import           Criterion.Main  (Benchmark, bench, defaultConfig, defaultMainWith, env,
                                  whnf)
import           Criterion.Types (Config (..))
import           Data.Maybe
import           Test.QuickCheck (generate)
import           Universum

import           Pos.Crypto      (SecretKey, hash, sign)
import           Pos.Types       (TxId, TxOut, TxSig)
import           Pos.Util        (arbitraryUnsafe)

{-
signTx :: (SecretKey, TxId, Word32, [TxOut]) -> TxSig
signTx (sk, thash, tidx, touts) = sign sk (thash, tidx, hash touts)

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
-}

runBenchmark :: IO ()
runBenchmark = return () --defaultMainWith txSignConfig [txSignBench]
