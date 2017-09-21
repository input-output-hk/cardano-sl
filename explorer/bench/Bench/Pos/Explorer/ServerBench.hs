module Bench.Pos.Explorer.ServerBench
    ( runTimeBenchmark
    , runSpaceBenchmark
    ) where

import           Universum

import           Criterion.Main               (Benchmark, bench, defaultConfig,
                                               defaultMainWith, env, whnf)
import           Criterion.Types              (Config (..))
import           Weigh                        (Weigh, io, mainWith)

import           Test.QuickCheck              (Arbitrary, arbitrary, generate)

import           Control.Lens                 (makeLenses)
import           Data.Default                 (def)
import           Pos.Arbitrary.Txp.Unsafe     ()
import           Pos.Ssc.GodTossing           (SscGodTossing)

import           Pos.Block.Core               (Block)
import           Pos.Block.Types              (Blund)
import           Pos.Core                     (HasCoreConstants, giveStaticConsts)
import           Pos.Types                    (HeaderHash, SlotLeaders, Timestamp)

import           Pos.Explorer.TestUtil        ()
import           Pos.Explorer.Web.ClientTypes (CBlockEntry, ExplorerMockMode (..))
import           Pos.Explorer.Web.Server      (getBlocksPageEMode)
import           Test.Pos.Block.Logic.Mode    (BlockTestMode, TestParams,
                                               runBlockTestMode)



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

instance NFData GeneratedTestArguments

-- TODO(ks): For now it's all arbitrary/random, I intend to remove this and make these
-- tests deterministic with several different options. For example how does it perform
-- when we have 100 leaders, 1000 leaders, and so on...
-- We are also missing some other options, since this is testing pure values without
-- their context - for example, we don't deserialize the database values which is
-- something that has a high cost. We do have the flexibility to add that though.
instance Arbitrary GeneratedTestArguments where
    arbitrary = do
        pageNumber  <- arbitrary
        tipBlock    <- giveStaticConsts arbitrary
        blockHHs    <- arbitrary
        blundsFHHs  <- giveStaticConsts arbitrary
        slotStart   <- arbitrary
        slotLeaders <- arbitrary

        pure $ GeneratedTestArguments
            { _gtaPageNumber              = pageNumber
            , _gtaTipBlock                = tipBlock
            , _gtaBlockHeaderHashes       = blockHHs
            , _gtaBlundsFromHeaderHashes  = blundsFHHs
            , _gtaSlotStart               = slotStart
            , _gtaSlotLeaders             = slotLeaders
            }

-- | The actual benched function.
getBlocksPageMock
    :: (HasCoreConstants)
    => GeneratedTestArguments
    -> IO (Integer, [CBlockEntry])
getBlocksPageMock genTestArgs = do
    testParams <- testParamsGen -- TODO(ks): Temporary test params, will be removed.
    runBlockTestMode testParams $ getBlocksPageEMode mode pageNumber 10
  where
    -- TODO(ks): Temporary test params, will be removed.
    testParamsGen :: IO TestParams
    testParamsGen = generate $ arbitrary

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

-- | Time @getBlocksPage@.
runTimeBenchmark :: IO ()
runTimeBenchmark = defaultMainWith getBlocksPageConfig [getBlocksPageBench]
  where
    -- | Configuration.
    getBlocksPageConfig :: Config
    getBlocksPageConfig = defaultConfig
        { reportFile = Just "bench/results/Server.html"
        }

    -- | Benchmark.
    getBlocksPageBench :: Benchmark
    getBlocksPageBench = env genArgs $ bench "Get blocks page" . whnf getBlocksPageMockBench
      where
        getBlocksPageMockBench :: GeneratedTestArguments -> IO (Integer, [CBlockEntry])
        getBlocksPageMockBench = giveStaticConsts getBlocksPageMock

        genArgs :: IO GeneratedTestArguments
        genArgs = generate $ arbitrary


-- | Space @getBlocksPage@.
runSpaceBenchmark :: IO ()
runSpaceBenchmark = mainWith =<< getPageBlocksMemory
  where
    -- | Just counting integers.
    getPageBlocksMemory :: IO (Weigh ())
    getPageBlocksMemory =
        io "getPageBlocksMemory" (giveStaticConsts getBlocksPageMock) <$> genArgs
      where
        genArgs :: IO GeneratedTestArguments
        genArgs = generate $ arbitrary

