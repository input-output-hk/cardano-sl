module Bench.Pos.Criterion.TxSigningBench
       ( runBenchmark
       ) where

import           Criterion.Main     (Benchmark, bench, defaultConfig, defaultMainWith,
                                     env, whnf)
import           Criterion.Types    (Config (..))
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Test.QuickCheck    (generate)
import           Universum

import           Pos.Crypto         (SecretKey, SignTag (SignTxIn), hash, sign)
import           Pos.Ssc.GodTossing ()
import           Pos.Txp            (TxDistribution (..), TxId, TxIn (..), TxOut, TxSig,
                                     TxSigData (..))
import           Pos.Util           (arbitraryUnsafe)

signTx :: (SecretKey, TxId, Word32, NonEmpty TxOut) -> TxSig
signTx (sk, thash, tidx, touts) = sign SignTxIn sk txSigData
  where
    distr = TxDistribution ([] :| replicate (length touts - 1) [])
    txSigData = TxSigData
        { txSigInput = TxIn thash tidx
        , txSigOutsHash = hash touts
        , txSigDistrHash = hash distr
        }

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
