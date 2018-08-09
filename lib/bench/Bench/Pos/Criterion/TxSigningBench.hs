module Bench.Pos.Criterion.TxSigningBench
       ( runBenchmark
       ) where

import           Criterion.Main (Benchmark, bench, defaultConfig, defaultMainWith, env, whnf)
import           Criterion.Types (Config (..))
import           Test.QuickCheck (generate)
import           Universum

import           Pos.Crypto (SecretKey, SignTag (SignTx), sign)
import           Pos.Ssc ()
import           Pos.Txp (TxId, TxSig, TxSigData (..))

import           Test.Pos.Txp.Arbitrary.Unsafe ()
import           Test.Pos.Util.QuickCheck.Arbitrary (arbitraryUnsafe)

import           Bench.Configuration (benchProtocolMagic)

signTx :: (SecretKey, TxId) -> TxSig
signTx (sk, thash) = sign benchProtocolMagic SignTx sk txSigData
  where
    txSigData = TxSigData
        { txSigTxHash = thash
        }

txSignBench :: Benchmark
txSignBench = env genArgs $ bench "Transactions signing" . whnf signTx
  where genArgs = generate $ (,)
                  <$> arbitraryUnsafe
                  <*> arbitraryUnsafe

txSignConfig :: Config
txSignConfig = defaultConfig
    { reportFile = Just "txSigning.html"
    }

runBenchmark :: IO ()
runBenchmark = defaultMainWith txSignConfig [txSignBench]
