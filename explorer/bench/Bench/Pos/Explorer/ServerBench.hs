module Bench.Pos.Explorer.ServerBench
    ( runTimeBenchmark
    , runSpaceBenchmark
    ) where

import           Universum

import           Criterion.Main (bench, defaultConfig, defaultMainWith, nfIO)
import           Criterion.Types (Config (..))
import           Weigh (io, mainWith)

import           Test.QuickCheck (arbitrary, generate)

import           Pos.Arbitrary.Txp.Unsafe ()

import           Test.Pos.Util (withDefConfigurations)

import           Pos.Explorer.ExplorerMode (ExplorerTestParams, runExplorerTestMode)
import           Pos.Explorer.ExtraContext (ExtraContext (..), makeMockExtraCtx)
import           Pos.Explorer.TestUtil (BlockNumber, SlotsPerEpoch,
                                        generateValidExplorerMockableMode)
import           Pos.Explorer.Web.ClientTypes (CBlockEntry)
import           Pos.Explorer.Web.Server (getBlocksPage, getBlocksTotal)


----------------------------------------------------------------
-- Mocked functions
----------------------------------------------------------------

type BenchmarkTestParams = (ExplorerTestParams, ExtraContext)

-- | @getBlocksTotal@ function for benchmarks.
getBlocksTotalWithParams
    :: BenchmarkTestParams
    -> IO Integer
getBlocksTotalWithParams (testParams, extraContext) =
    withDefConfigurations $
        runExplorerTestMode testParams extraContext getBlocksTotal

-- | @getBlocksPage@ function for the last page for benchmarks.
getBlocksPageWithParams
    :: BenchmarkTestParams
    -> IO (Integer, [CBlockEntry])
getBlocksPageWithParams (testParams, extraContext) =
    withDefConfigurations $
        runExplorerTestMode testParams extraContext $ getBlocksPage Nothing (Just 10)

-- | This is used to generate the test environment. We don't do this while benchmarking
-- the functions since that would include the time/memory required for the generation of the
-- mock blockchain (test environment), and we don't want to include that in our benchmarks.
generateTestEnv
    :: BlockNumber
    -> SlotsPerEpoch
    -> IO (ExplorerTestParams, ExtraContext)
generateTestEnv totalBlocksNumber slotsPerEpoch = do
    testParams <- testParamsGen

    -- We replace the "real" blockchain with our custom generated one.
    mode <- generateValidExplorerMockableMode totalBlocksNumber slotsPerEpoch

    -- The extra context so we can mock the functions.
    let extraContext :: ExtraContext
        extraContext = withDefConfigurations $ makeMockExtraCtx mode

    pure (testParams, extraContext)
  where
    -- | Generated test parameters.
    testParamsGen :: IO ExplorerTestParams
    testParamsGen = generate arbitrary


----------------------------------------------------------------
-- Time benchmark
----------------------------------------------------------------

-- | Time @getBlocksPage@.
{-# ANN funcName ("HLint: ignore Reduce duplication" :: Text) #-}
runTimeBenchmark :: IO ()
runTimeBenchmark = do
    -- Generate the test environment before the benchmarks.
    blocks100   <- generateTestEnv 100 10
    blocks1000  <- generateTestEnv 1000 10
    blocks10000 <- generateTestEnv 10000 10

    defaultMainWith getBlocksPageConfig
        [ bench "getBlocksTotal 100 blocks" $ nfIO $ getBlocksTotalWithParams blocks100
        , bench "getBlocksTotal 1000 blocks" $ nfIO $ getBlocksTotalWithParams blocks1000
        , bench "getBlocksTotal 10000 blocks" $ nfIO $ getBlocksTotalWithParams blocks10000

        , bench "getBlocksPage 100 blocks" $ nfIO $ getBlocksPageWithParams blocks100
        , bench "getBlocksPage 1000 blocks" $ nfIO $ getBlocksPageWithParams blocks1000
        , bench "getBlocksPage 10000 blocks" $ nfIO $ getBlocksPageWithParams blocks10000
        ]

  where
    -- | Configuration.
    getBlocksPageConfig :: Config
    getBlocksPageConfig = defaultConfig
        { reportFile = Just "bench/results/ServerBackend.html"
        }

----------------------------------------------------------------
-- Space benchmark
----------------------------------------------------------------

-- | Space @getBlocksPage@.
{-# ANN funcName ("HLint: ignore Reduce duplication" :: Text) #-}
runSpaceBenchmark :: IO ()
runSpaceBenchmark = do
    -- Generate the test environment before the benchmarks.
    blocks100   <- generateTestEnv 100 10
    blocks1000  <- generateTestEnv 1000 10
    blocks10000 <- generateTestEnv 10000 10

    mainWith $ do
        io "getBlocksTotal 100 blocks" getBlocksTotalWithParams blocks100
        io "getBlocksTotal 1000 blocks" getBlocksTotalWithParams blocks1000
        io "getBlocksTotal 10000 blocks" getBlocksTotalWithParams blocks10000

        io "getBlocksPage 100 blocks" getBlocksPageWithParams blocks100
        io "getBlocksPage 1000 blocks" getBlocksPageWithParams blocks1000
        io "getBlocksPage 10000 blocks" getBlocksPageWithParams blocks10000
