module Bench.Pos.Criterion.TxSigningBench
       ( runBenchmark
       ) where

import           Criterion.Main (Benchmark, bench, defaultConfig, defaultMainWith, env, whnf)
import           Criterion.Types (Config (..))
import           Test.QuickCheck (generate)
import           Universum

import           Pos.Arbitrary.Txp.Unsafe ()
import           Pos.Core (HasConfiguration)
import           Pos.Crypto (SecretKey, SignTag (SignTx), sign, protocolMagic)
import           Pos.Ssc ()
import           Pos.Txp (TxId, TxSig, TxSigData (..))
import           Pos.Util (arbitraryUnsafe)

import           Bench.Configuration (giveCoreConf)

signTx :: HasConfiguration => (SecretKey, TxId) -> TxSig
signTx (sk, thash) = sign protocolMagic SignTx sk txSigData
  where
    txSigData = TxSigData
        { txSigTxHash = thash
        }

txSignBench :: HasConfiguration => Benchmark
txSignBench = env genArgs $ bench "Transactions signing" . whnf signTx
  where genArgs = generate $ (,)
                  <$> arbitraryUnsafe
                  <*> arbitraryUnsafe

txSignConfig :: Config
txSignConfig = defaultConfig
    { reportFile = Just "txSigning.html"
    }

runBenchmark :: IO ()
runBenchmark = giveCoreConf $ defaultMainWith txSignConfig [txSignBench]
