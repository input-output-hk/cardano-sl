module Bench.Pos.Criterion.FollowTheSatoshiBench
    ( runBenchmark
    ) where

import           Criterion.Main      (Benchmark, bench, defaultConfig, defaultMainWith,
                                      env, whnf)
import           Criterion.Types     (Config (..))
import           Data.Map            (fromList)
import           Formatting          (int, sformat, (%))
import           Test.QuickCheck     (Arbitrary (..), Gen, generate, infiniteListOf)
import           Universum

import           Pos.Core            (HasConfiguration)
import           Pos.Lrc             (followTheSatoshi)
import           Pos.Types           (Utxo)
import           Pos.Util            (arbitraryUnsafe)

import           Bench.Configuration (giveCoreConf)

type UtxoSize = Int

-- [CSL-192]: make a trick and use special unsafe arbitrary instances
-- for generation of such things
arbitraryUtxoOfSize :: UtxoSize -> Gen Utxo
arbitraryUtxoOfSize n = fromList . take n <$> infiniteListOf arbitraryUnsafe

ftsBench :: HasConfiguration => UtxoSize -> Benchmark
ftsBench n = env genArgs $ bench msg . whnf (uncurry followTheSatoshi)
    where genArgs = generate $ (,) <$> arbitrary <*> arbitraryUtxoOfSize n
          msg = toString $ sformat ("followTheSatoshi: Utxo of size "%int) n

ftsConfig :: Config
ftsConfig = defaultConfig
    { reportFile = Just "followTheSatoshi.html"
    }

runBenchmark :: IO ()
runBenchmark = giveCoreConf $ defaultMainWith ftsConfig $ map ftsBench [1000, 10000, 100000]
