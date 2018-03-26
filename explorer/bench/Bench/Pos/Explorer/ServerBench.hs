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

import           Test.Pos.Configuration (withDefConfigurations)

import           Pos.Explorer.DB (defaultPageSize)
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
getBlocksTotalBench
    :: BenchmarkTestParams
    -> IO Integer
getBlocksTotalBench (testParams, extraContext) =
    withDefConfigurations $ \_ ->
        runExplorerTestMode testParams extraContext getBlocksTotal

-- | @getBlocksPage@ function for the last page for benchmarks.
getBlocksPageBench
    :: BenchmarkTestParams
    -> IO (Integer, [CBlockEntry])
getBlocksPageBench (testParams, extraContext) =
    withDefConfigurations $ \_ ->
        runExplorerTestMode testParams extraContext $
            getBlocksPage Nothing (Just $ fromIntegral defaultPageSize)

-- | This is used to generate the test environment. We don't do this while benchmarking
-- the functions since that would include the time/memory required for the generation of the
-- mock blockchain (test environment), and we don't want to include that in our benchmarks.
generateTestParams
    :: BlockNumber
    -> SlotsPerEpoch
    -> IO BenchmarkTestParams
generateTestParams totalBlocksNumber slotsPerEpoch = do
    testParams <- testParamsGen

    -- We replace the "real" blockchain with our custom generated one.
    mode <- generateValidExplorerMockableMode totalBlocksNumber slotsPerEpoch

    -- The extra context so we can mock the functions.
    let extraContext :: ExtraContext
        extraContext = withDefConfigurations $ const $ makeMockExtraCtx mode

    pure (testParams, extraContext)
  where
    -- | Generated test parameters.
    testParamsGen :: IO ExplorerTestParams
    testParamsGen = generate arbitrary

-- | Extracted common code. This needs to be run before the benchmarks since we don't
-- want to include time/memory of the test data generation in the benchmarks.
usingGeneratedBlocks :: IO (BenchmarkTestParams, BenchmarkTestParams, BenchmarkTestParams)
usingGeneratedBlocks = do

    blocks100   <- generateTestParams 100 10
    blocks1000  <- generateTestParams 1000 10
    blocks10000 <- generateTestParams 10000 10

    pure (blocks100, blocks1000, blocks10000)

----------------------------------------------------------------
-- Time benchmark
----------------------------------------------------------------

-- | Time @getBlocksPage@.
runTimeBenchmark :: IO ()
runTimeBenchmark = do
    -- Generate the test environment before the benchmarks.
    (blocks100, blocks1000, blocks10000) <- usingGeneratedBlocks

    defaultMainWith getBlocksPageConfig
        [ bench "getBlocksTotal 100 blocks" $ nfIO $ getBlocksTotalBench blocks100
        , bench "getBlocksTotal 1000 blocks" $ nfIO $ getBlocksTotalBench blocks1000
        , bench "getBlocksTotal 10000 blocks" $ nfIO $ getBlocksTotalBench blocks10000

        , bench "getBlocksPage 100 blocks" $ nfIO $ getBlocksPageBench blocks100
        , bench "getBlocksPage 1000 blocks" $ nfIO $ getBlocksPageBench blocks1000
        , bench "getBlocksPage 10000 blocks" $ nfIO $ getBlocksPageBench blocks10000
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
runSpaceBenchmark :: IO ()
runSpaceBenchmark = do
    -- Generate the test environment before the benchmarks.
    (blocks100, blocks1000, blocks10000) <- usingGeneratedBlocks

    mainWith $ do
        io "getBlocksTotal 100 blocks" getBlocksTotalBench blocks100
        io "getBlocksTotal 1000 blocks" getBlocksTotalBench blocks1000
        io "getBlocksTotal 10000 blocks" getBlocksTotalBench blocks10000

        io "getBlocksPage 100 blocks" getBlocksPageBench blocks100
        io "getBlocksPage 1000 blocks" getBlocksPageBench blocks1000
        io "getBlocksPage 10000 blocks" getBlocksPageBench blocks10000
