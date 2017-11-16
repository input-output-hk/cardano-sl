module Bench.Pos.Explorer.ServerBench
    ( runTimeBenchmark
    , runSpaceBenchmark
    ) where

import qualified Prelude
import           Universum

import           Criterion.Main (Benchmark, bench, defaultConfig, defaultMainWith, env, whnf)
import           Criterion.Types (Config (..))
import           Weigh (Weigh, io, mainWith)

import           Test.QuickCheck (arbitrary, generate)

import           Control.Lens (makeLenses)
import           Data.Default (def)
import           Pos.Arbitrary.Txp.Unsafe ()

import           Pos.Block.Types (Blund)
import           Pos.Core (HeaderHash, SlotLeaders, Timestamp)
import           Pos.Core.Block (Block)
import           Pos.Launcher.Configuration (HasConfigurations)
import           Test.Pos.Util (withDefConfigurations)

import           Pos.Explorer.ExplorerMode (ExplorerTestParams, runExplorerTestMode)
import           Pos.Explorer.ExtraContext (ExplorerMockableMode (..), ExtraContext (..),
                                            makeMockExtraCtx)
import           Pos.Explorer.TestUtil (produceBlocksByBlockNumberAndSlots, produceSecretKeys,
                                        produceSlotLeaders)
import           Pos.Explorer.Web.ClientTypes (CBlockEntry)
import           Pos.Explorer.Web.Server (getBlocksPage, getBlocksTotal)


----------------------------------------------------------------
-- Function mocks
----------------------------------------------------------------

-- | The data structure that contains all required generated parameters to test the
-- function.
data GeneratedTestArguments = GeneratedTestArguments
    { _gtaPageNumber             :: Maybe Word
    , _gtaTipBlock               :: Block
    , _gtaBlockHeaderHashes      :: [HeaderHash]
    , _gtaBlundsFromHeaderHashes :: Blund
    , _gtaSlotStart              :: Timestamp
    , _gtaSlotLeaders            :: SlotLeaders
    } deriving (Generic)

makeLenses ''GeneratedTestArguments

instance HasConfigurations => NFData GeneratedTestArguments

-- | More predictable generation that doesn't violate the invariants.
genArgs :: Word -> Word -> Word -> IO GeneratedTestArguments
genArgs blocksNumber slotsPerEpoch pageNumber = do

    slotStart     <- liftIO $ generate $ arbitrary

    blockHHs      <- liftIO $ generate $ arbitrary
    blundsFHHs    <- liftIO $ generate $ withDefConfigurations arbitrary

    slotLeaders   <- produceSlotLeaders blocksNumber
    secretKeys    <- produceSecretKeys blocksNumber

    blocks <- withDefConfigurations $
        produceBlocksByBlockNumberAndSlots blocksNumber slotsPerEpoch slotLeaders secretKeys

    let tipBlock = Prelude.last blocks

    pure $ GeneratedTestArguments
        { _gtaPageNumber              = Just pageNumber
        , _gtaTipBlock                = tipBlock
        , _gtaBlockHeaderHashes       = blockHHs
        , _gtaBlundsFromHeaderHashes  = blundsFHHs
        , _gtaSlotStart               = slotStart
        , _gtaSlotLeaders             = slotLeaders
        }

----------------------------------------------------------------
-- Mocked functions
----------------------------------------------------------------

getBlocksTotalMock
    :: (HasConfigurations)
    => GeneratedTestArguments
    -> IO Integer
getBlocksTotalMock genTestArgs = do
    testParams <- testParamsGen

    -- We replace the "real" database with our custom data.
    let mode :: ExplorerMockableMode
        mode = def { emmGetTipBlock = pure $ genTestArgs ^. gtaTipBlock }

    -- The extra context so we can mock the functions.
    let extraContext :: ExtraContext
        extraContext = makeMockExtraCtx mode

    runExplorerTestMode testParams extraContext getBlocksTotal


-- | The actual benched function.
getBlocksPageMock
    :: (HasConfigurations)
    => GeneratedTestArguments
    -> IO (Integer, [CBlockEntry])
getBlocksPageMock genTestArgs = do
    testParams <- testParamsGen -- TODO(ks): Temporary test params, will be removed.

        -- We replace the "real" database with our custom data.
    let mode :: ExplorerMockableMode
        mode = def {
            emmGetTipBlock         = pure $ genTestArgs ^. gtaTipBlock,
            emmGetPageBlocks       = \_ -> pure $ Just $ genTestArgs ^. gtaBlockHeaderHashes,
            emmGetBlundFromHH      = \_ -> pure $ Just $ genTestArgs ^. gtaBlundsFromHeaderHashes,
            emmGetSlotStart        = \_ -> pure $ Just $ genTestArgs ^. gtaSlotStart,
            emmGetLeadersFromEpoch = \_ -> pure $ Just $ genTestArgs ^. gtaSlotLeaders
        }

    -- The extra context so we can mock the functions.
    let extraContext :: ExtraContext
        extraContext = makeMockExtraCtx mode

    runExplorerTestMode testParams extraContext $ getBlocksPage pageNumber (Just 10)
  where
    -- The page number we send to the function
    pageNumber :: Maybe Word
    pageNumber = genTestArgs ^. gtaPageNumber


-- TODO(ks): Temporary test params, will be removed.
testParamsGen :: IO ExplorerTestParams
testParamsGen = generate arbitrary

----------------------------------------------------------------
-- Time benchmark
----------------------------------------------------------------

-- | Time @getBlocksPage@.
runTimeBenchmark :: IO ()
runTimeBenchmark = defaultMainWith getBlocksPageConfig
    [ get100BlocksTotalMock
    , get100BlocksPageBench
    , get1000BlocksPageBench
    , get10000BlocksPageBench
    ]
  where
    -- | Configuration.
    getBlocksPageConfig :: Config
    getBlocksPageConfig = defaultConfig
        { reportFile = Just "bench/results/ServerBackend.html"
        }

    get100BlocksTotalMock :: Benchmark
    get100BlocksTotalMock = withDefConfigurations $
        env (genArgs 100 50 2) $ bench "Get total blocks" . whnf getBlocksTotalMockBench
      where
        getBlocksTotalMockBench :: GeneratedTestArguments -> IO Integer
        getBlocksTotalMockBench = withDefConfigurations getBlocksTotalMock

    get100BlocksPageBench :: Benchmark
    get100BlocksPageBench = withDefConfigurations $
        env (genArgs 100 50 2) $ bench "Get 100 blocks page" . whnf getBlocksPageMockBench
      where
        getBlocksPageMockBench :: GeneratedTestArguments -> IO (Integer, [CBlockEntry])
        getBlocksPageMockBench = withDefConfigurations getBlocksPageMock

    get1000BlocksPageBench :: Benchmark
    get1000BlocksPageBench = withDefConfigurations $
        env (genArgs 1000 50 2) $ bench "Get 1000 blocks page" . whnf getBlocksPageMockBench
      where
        getBlocksPageMockBench :: GeneratedTestArguments -> IO (Integer, [CBlockEntry])
        getBlocksPageMockBench = withDefConfigurations getBlocksPageMock

    get10000BlocksPageBench :: Benchmark
    get10000BlocksPageBench = withDefConfigurations $
        env (genArgs 10000 50 2) $ bench "Get 10000 blocks page" . whnf getBlocksPageMockBench
      where
        getBlocksPageMockBench :: GeneratedTestArguments -> IO (Integer, [CBlockEntry])
        getBlocksPageMockBench = withDefConfigurations getBlocksPageMock


----------------------------------------------------------------
-- Space benchmark
----------------------------------------------------------------

-- | Space @getBlocksPage@.
runSpaceBenchmark :: IO ()
runSpaceBenchmark = mapM_ (mainWith =<<)
    [ get100BlocksTotalMock
    , get100BlocksPageBench
    , get1000BlocksPageBench
    , get10000BlocksPageBench
    ]
  where
    get100BlocksTotalMock :: IO (Weigh ())
    get100BlocksTotalMock = do
        io "getBlocksPageBench" (withDefConfigurations getBlocksTotalMock)
            <$> genArgs 100 50 2

    get100BlocksPageBench :: IO (Weigh ())
    get100BlocksPageBench = do
        io "getBlocksPageBench" (withDefConfigurations getBlocksPageMock)
            <$> genArgs 100 50 2

    get1000BlocksPageBench :: IO (Weigh ())
    get1000BlocksPageBench = do
        io "getBlocksPageBench" (withDefConfigurations getBlocksPageMock)
            <$> genArgs 1000 50 2

    get10000BlocksPageBench :: IO (Weigh ())
    get10000BlocksPageBench = do
        io "getBlocksPageBench" (withDefConfigurations getBlocksPageMock)
            <$> genArgs 10000 50 2
