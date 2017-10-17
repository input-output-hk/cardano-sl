module Bench.Pos.Explorer.ServerBench
    ( runTimeBenchmark
    , runSpaceBenchmark
    ) where

import qualified Prelude
import           Universum

-- import           Formatting                        (bprint)  --(bprint, build, int, stext, (%))
import           Criterion.Main               (Benchmark, bench, defaultConfig,
                                               defaultMainWith, env, whnf)
import           Criterion.Types              (Config (..))
import           Weigh                        (Weigh, io, mainWith)

import           Test.QuickCheck              (arbitrary, generate)

import           Control.Lens                 (makeLenses)
import           Data.Default                 (def)
import           Pos.Arbitrary.Txp.Unsafe     ()
import           Pos.Ssc.GodTossing           (SscGodTossing)

import           Pos.Block.Core               (Block)
import           Pos.Block.Types              (Blund)
import           Pos.Launcher.Configuration   (HasConfigurations)
import           Test.Pos.Util                (withDefConfigurations)
import           Pos.Types                    (HeaderHash, SlotLeaders, Timestamp)

import           Pos.Explorer.TestUtil        (produceBlocksByBlockNumberAndSlots,
                                               produceSecretKeys, produceSlotLeaders)
import           Pos.Explorer.Web.ClientTypes (CBlockEntry, ExplorerMockMode (..))
import           Pos.Explorer.Web.Server      (getBlocksTotalEMode, getBlocksPageEMode)
import           Test.Pos.Block.Logic.Mode    (BlockTestMode, TestParams,
                                               runBlockTestMode)

----------------------------------------------------------------
-- Function mocks
----------------------------------------------------------------

-- | The data structure that contains all required generated parameters to test the
-- function.
data GeneratedTestArguments = GeneratedTestArguments
    { _gtaPageNumber              :: Maybe Word
    , _gtaTipBlock                :: Block SscGodTossing
    , _gtaBlockHeaderHashes       :: {-Maybe -}[HeaderHash]
    , _gtaBlundsFromHeaderHashes  :: {-Maybe -}(Blund SscGodTossing)
    , _gtaSlotStart               :: {-Maybe -}Timestamp
    , _gtaSlotLeaders             :: {-Maybe -}SlotLeaders
    } deriving (Generic)

makeLenses ''GeneratedTestArguments

instance HasConfigurations => NFData GeneratedTestArguments

-- | More predictable generated arguments.
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
    runBlockTestMode testParams $ getBlocksTotalEMode mode
  where
    -- TODO(ks): Temporary test params, will be removed.
    testParamsGen :: IO TestParams
    testParamsGen = generate arbitrary

    defaultInstance :: ExplorerMockMode BlockTestMode SscGodTossing
    defaultInstance = def

    -- The mocked CSL function interfaces
    mode :: ExplorerMockMode BlockTestMode SscGodTossing
    mode = defaultInstance { emmGetTipBlock = pure $ genTestArgs ^. gtaTipBlock }


-- | The actual benched function.
getBlocksPageMock
    :: (HasConfigurations)
    => GeneratedTestArguments
    -> IO (Integer, [CBlockEntry])
getBlocksPageMock genTestArgs = do
    testParams <- testParamsGen -- TODO(ks): Temporary test params, will be removed.
    runBlockTestMode testParams $ getBlocksPageEMode mode pageNumber 10
  where
    -- TODO(ks): Temporary test params, will be removed.
    testParamsGen :: IO TestParams
    testParamsGen = generate arbitrary

    -- The page number we send to the function
    pageNumber :: Maybe Word
    pageNumber = genTestArgs ^. gtaPageNumber

    defaultInstance :: ExplorerMockMode BlockTestMode SscGodTossing
    defaultInstance = def

    -- The mocked CSL function interfaces
    mode :: ExplorerMockMode BlockTestMode SscGodTossing
    mode = defaultInstance {
        emmGetTipBlock            = pure $ genTestArgs ^. gtaTipBlock,
        emmGetPageBlocks          = \_ -> pure $ Just $ genTestArgs ^. gtaBlockHeaderHashes,
        emmGetBlundFromHH         = \_ -> pure $ Just $ genTestArgs ^. gtaBlundsFromHeaderHashes,
        emmGetSlotStart           = \_ -> pure $ Just $ genTestArgs ^. gtaSlotStart,
        emmGetLeadersFromEpoch    = \_ -> pure $ Just $ genTestArgs ^. gtaSlotLeaders
    }

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