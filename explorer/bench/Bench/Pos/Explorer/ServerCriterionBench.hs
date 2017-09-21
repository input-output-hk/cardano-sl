module Bench.Pos.Explorer.ServerCriterionBench
    ( runBenchmark
    ) where

import           Universum

import           Criterion.Main               (Benchmark, bench, defaultConfig,
                                               defaultMainWith, env, whnf)
import           Criterion.Types              (Config (..))
import           Test.QuickCheck              (Arbitrary, generate, arbitrary)

import           Control.Lens                 (makeLenses)
import           Data.Default                 (def)
import           Pos.Arbitrary.Txp.Unsafe     ()
import           Pos.Ssc.GodTossing           (SscGodTossing)

-- Required types, maybe we can re-export them?
import           Pos.Core                     (HasCoreConstants, giveStaticConsts)
import           Pos.Block.Core               (Block)
import           Pos.Types                    (HeaderHash, SlotLeaders, Timestamp)
import           Pos.Block.Types              (Blund)

import           Pos.Explorer.TestUtil        ()
import           Test.Pos.Block.Logic.Mode    (TestParams, BlockTestMode, runBlockTestMode)
import           Pos.Explorer.Web.ClientTypes (ExplorerMockMode (..), CBlockEntry)
import           Pos.Explorer.Web.Server      (getBlocksPageEMode)



-- | The data structure that contains all required generated parameters to test the
-- function.
data GeneratedTestArguments = GeneratedTestArguments
    { _gtaPageNumber              :: Maybe Word
    , _gtaTipBlock                :: Block SscGodTossing
    , _gtaBlockHeaderHashes       :: Maybe [HeaderHash]
    , _gtaBlundsFromHeaderHashes  :: Maybe (Blund SscGodTossing)
    , _gtaSlotStart               :: Maybe Timestamp
    , _gtaSlotLeaders             :: Maybe SlotLeaders
    } deriving (Generic)

makeLenses ''GeneratedTestArguments

instance NFData GeneratedTestArguments

-- TODO(ks): For now it's all arbitrary/random, I intend to remove this and make these
-- tests deterministic with several different options. For example how does it perform
-- when we have 100 leaders, 1000 leaders, and so on...
instance Arbitrary GeneratedTestArguments where
    arbitrary = do
        --pageNumber  <- choose (0, 100) -- Let's simplify and say we have "just" 100 pages.
        pageNumber  <- arbitrary

        -- header      <- arbitrary
        -- sk          <- arbitrary
        -- slotId      <- arbitrary

        --tipBlock    <- basicBlockGenericUnsafe header sk slotId
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

getBlocksPageMock
    :: (HasCoreConstants)
    => GeneratedTestArguments
    -> IO (Integer, [CBlockEntry])
getBlocksPageMock genTestArgs = do
    testParams <- testParamsGen
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
        emmGetPageBlocks          = \_ -> pure $ genTestArgs ^. gtaBlockHeaderHashes,
        emmGetBlundFromHH         = \_ -> pure $ genTestArgs ^. gtaBlundsFromHeaderHashes,
        emmGetSlotStart           = \_ -> pure $ genTestArgs ^. gtaSlotStart,
        emmGetLeadersFromEpoch    = \_ -> pure $ genTestArgs ^. gtaSlotLeaders
    }

getBlocksPageBench :: HasCoreConstants => Benchmark
getBlocksPageBench = env genArgs $ bench "Get blocks page" . whnf getBlocksPageMock
  where
    genArgs :: IO GeneratedTestArguments
    genArgs = generate $ arbitrary

getBlocksPageConfig :: Config
getBlocksPageConfig = defaultConfig
    { reportFile = Just "getBlocksPage.html"
    }

runBenchmark :: HasCoreConstants => IO ()
runBenchmark = defaultMainWith getBlocksPageConfig [getBlocksPageBench]
