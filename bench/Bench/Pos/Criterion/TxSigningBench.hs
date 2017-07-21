module Bench.Pos.Criterion.TxSigningBench
       ( runBenchmark
       ) where

import           Criterion.Main           (Benchmark, bench, defaultConfig,
                                           defaultMainWith, env, whnf)
import           Criterion.Types          (Config (..))
import           Data.List.NonEmpty       (NonEmpty ((:|)))
import           Test.QuickCheck          (generate)
import           Universum

import           Pos.Crypto               (SecretKey, SignTag (SignTx), hash, sign)
import           Pos.Ssc.GodTossing       ()
import           Pos.Txp                  (TxDistribution (..), TxId, TxOut, TxSig,
                                           TxSigData (..))
import           Pos.Txp.Arbitrary.Unsafe ()
import           Pos.Util                 (arbitraryUnsafe)

signTx :: (SecretKey, TxId, NonEmpty TxOut) -> TxSig
signTx (sk, thash, touts) = sign SignTx sk txSigData
  where
    distr = TxDistribution ([] :| replicate (length touts - 1) [])
    txSigData = TxSigData
        { txSigTxHash = thash
        , txSigTxDistrHash = hash distr
        }

txSignBench :: Benchmark
txSignBench = env genArgs $ bench "Transactions signing" . whnf signTx
  where genArgs = generate $ (,,)
                  <$> arbitraryUnsafe
                  <*> arbitraryUnsafe
                  <*> arbitraryUnsafe

txSignConfig :: Config
txSignConfig = defaultConfig
    { reportFile = Just "txSigning.html"
    }

runBenchmark :: IO ()
runBenchmark = defaultMainWith txSignConfig [txSignBench]
