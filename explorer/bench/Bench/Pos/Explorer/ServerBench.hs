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

import           Test.QuickCheck              (Arbitrary, arbitrary, generate)

import           Control.Lens                 (makeLenses)
import           Data.Default                 (def)
import           Pos.Arbitrary.Txp.Unsafe     ()
import           Pos.Ssc.GodTossing           (SscGodTossing)

import           Pos.Block.Core               (Block)
import           Pos.Block.Types              (Blund)
import           Pos.Core                     (HasCoreConstants, giveStaticConsts)
import           Pos.Types                    (HeaderHash, SlotLeaders, Timestamp)

import           Pos.Explorer.TestUtil        (produceBlocksByBlockNumberAndSlots,
                                               produceSecretKeys, produceSlotLeaders)
import           Pos.Explorer.Web.ClientTypes (CBlockEntry, ExplorerMockMode (..))
import           Pos.Explorer.Web.Server      (pureGetBlocksTotal, getBlocksPageEMode)
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

-- | Pass a page number and return a function that creates a block after we pass it
-- the previous block header, secret key and slot id.
{-
createTipBlock
    :: (HasCoreConstants)
    => Word
    -> BlockHeader SscGodTossing
    -> SecretKey
    -> SlotId
    -> Block SscGodTossing
createTipBlock pageNumber = undefined

basicBlockGenericUnsafe
    :: (HasCoreConstants)
    => BlockHeader SscGodTossing
    -> SecretKey
    -> SlotId
    -> Block SscGodTossing
basicBlockGenericUnsafe prevHeader sk slotId = case (basicBlock prevHeader sk slotId) of
    Left e      -> error e
    Right block -> Right block

basicBlock
    :: (HasCoreConstants)
    => BlockHeader SscGodTossing
    -> SecretKey
    -> SlotId
    -> Either Text (MainBlock SscGodTossing)
basicBlock prevHeader sk slotId =
    producePureBlock infLimit prevHeader [] Nothing slotId def (defGTP slotId) def sk
  where
    defGTP :: HasCoreConstants => SlotId -> GtPayload
    defGTP sId = sscDefaultPayload @SscGodTossing $ siSlot sId

    infLimit :: Byte
    infLimit = convertUnit @Gigabyte @Byte 1
-}

-- | Space @getBlocksPage@.
runSpaceBenchmark :: IO ()
runSpaceBenchmark = mainWith =<< getPageBlocksMemory
  where
    -- | Just counting integers.
    getPageBlocksMemory :: IO (Weigh ())
    getPageBlocksMemory =
        io "getPageBlocksMemory" (giveStaticConsts getBlocksPageMock) <$> genArgs

-- | More predictable genereted arguments.
genArgs :: IO GeneratedTestArguments
genArgs = do
    let blocksNumber  = 3000
    let pageNumber    = 15
    let slotsPerEpoch = 1000

    slotStart     <- liftIO $ generate $ arbitrary

    blockHHs      <- liftIO $ generate $ arbitrary
    blundsFHHs    <- liftIO $ generate $ giveStaticConsts arbitrary

    slotLeaders   <- produceSlotLeaders blocksNumber
    secretKeys    <- produceSecretKeys blocksNumber

    blocks <-
        giveStaticConsts $ produceBlocksByBlockNumberAndSlots blocksNumber slotsPerEpoch slotLeaders secretKeys

    let tipBlock = Prelude.last blocks
    -- let tipBlock = case  of
    --                     Nothing    -> error "Cannot find last block."
    --                     Just block -> block

    -- liftIO $ putStrLn $ bprint tipBlock

    liftIO $ putStrLn $ (show $ length blocks :: String)
    liftIO $ putStrLn $ (show (pureGetBlocksTotal tipBlock :: Integer) :: String)

    pure $ GeneratedTestArguments
        { _gtaPageNumber              = Just pageNumber
        , _gtaTipBlock                = tipBlock
        , _gtaBlockHeaderHashes       = blockHHs
        , _gtaBlundsFromHeaderHashes  = blundsFHHs
        , _gtaSlotStart               = slotStart
        , _gtaSlotLeaders             = slotLeaders
        }