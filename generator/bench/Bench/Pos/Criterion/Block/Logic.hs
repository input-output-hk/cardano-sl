module Bench.Pos.Criterion.Block.Logic
    ( runBenchmark
    ) where

import           Universum

import           Control.Monad.Random.Strict (evalRandT, mapRandT)
import           Criterion.Main (Benchmark, Benchmarkable, bench, bgroup,
                     defaultConfig, defaultMainWith, env, nf, nfIO)
import           Criterion.Types as Criterion (Config (..))
import qualified Data.List.NonEmpty as NE
import           Data.Time.Units (convertUnit)
import           Serokell.Util.Verify (isVerSuccess)
import           System.Random (newStdGen)

import           Pos.AllSecrets (mkAllSecretsSimple)
import           Pos.Chain.Block (Block, VerifyBlockParams (..),
                     VerifyHeaderParams (..), getBlockHeader, verifyBlock,
                     verifyHeader)
import           Pos.Core as Core (Config (..), configBlockVersionData,
                     configBootStakeholders, configEpochSlots,
                     configGeneratedSecretsThrow)
import           Pos.Core.Chrono (NE, OldestFirst (..), nonEmptyNewestFirst)
import           Pos.Core.Common (BlockCount (..), unsafeCoinPortionFromDouble)
import           Pos.Core.Genesis (FakeAvvmOptions (..),
                     GenesisInitializer (..), TestnetBalanceOptions (..),
                     gsSecretKeys)
import           Pos.Core.Slotting (EpochOrSlot (..), SlotId, Timestamp (..),
                     epochIndexL, getEpochOrSlot)
import           Pos.Core.Update (BlockVersionData (..))
import           Pos.Crypto (SecretKey)
import           Pos.DB (getTipHeader)
import           Pos.DB.Block (rollbackBlocks, verifyAndApplyBlocks,
                     verifyBlocksPrefix)
import           Pos.DB.DB (initNodeDBs)
import           Pos.DB.Lrc (getLeadersForEpoch, lrcActionOnEpochReason)
import           Pos.DB.Txp (txpGlobalSettings)
import           Pos.Generator.Block (BlockGenParams (..), TxGenParams (..),
                     genBlockNoApply, genBlocks, mkBlockGenContext)
import           Pos.Launcher.Configuration (ConfigurationOptions (..),
                     HasConfigurations, defaultConfigurationOptions,
                     withConfigurationsM)
import           Pos.Util.CompileInfo (withCompileInfo)
import           Pos.Util.Util (realTime)
import           Pos.Util.Wlog (LoggerName (..))

import           Test.Pos.Block.Logic.Emulation (runEmulation, sudoLiftIO)
import           Test.Pos.Block.Logic.Mode (BlockTestContext, BlockTestMode,
                     TestParams (..), initBlockTestContext, runBlockTestMode)
import           Test.Pos.Block.Logic.Util (satisfySlotCheck)

-- | Criterion configuration
config :: Criterion.Config
config = defaultConfig
    { reportFile = Just "verification.html"
    }

genesisInitializer :: GenesisInitializer
genesisInitializer = GenesisInitializer
    { giTestBalance = balance
    , giFakeAvvmBalance = FakeAvvmOptions
          { faoCount = 1
          , faoOneBalance = maxBound
          }
    , giAvvmBalanceFactor = unsafeCoinPortionFromDouble 0
    , giUseHeavyDlg = False
    , giSeed = 0
    }

balance :: TestnetBalanceOptions
balance = TestnetBalanceOptions
    { tboPoors = 1
    , tboRichmen = 1
    , tboTotalBalance = maxBound
    , tboRichmenShare = 1
    , tboUseHDAddresses = False
    }

runBTM
    :: TestParams
    -> BlockTestContext
    -> BlockTestMode a
    -> IO a
runBTM tp ctx btm = runEmulation (getTimestamp (_tpStartTime tp)) $ runReaderT btm ctx

-- | Benchmark which runs `verifyAndApplyBlocks` && `rollbackBlocks`.
verifyBlocksBenchmark
    :: HasConfigurations
    => Core.Config
    -> [SecretKey]
    -> TestParams
    -> BlockTestContext
    -> Benchmark
verifyBlocksBenchmark !coreConfig !secretKeys !tp !ctx =
    bgroup "block verification"
        [ env (runBlockTestMode coreConfig tp (genEnv (BlockCount 100)))
            $ \ ~(curSlot, blocks) -> bench "verifyAndApplyBlocks" (verifyAndApplyBlocksB curSlot blocks)
        -- `verifyBlocksPrefix` will succeed only on the first block, it
        -- requires that blocks are applied.
        , env (runBlockTestMode coreConfig tp (genEnv (BlockCount 1)))
            $ \ ~(curSlot, blocks) -> bench "verifyBlocksPrefix" (verifyBlocksPrefixB curSlot blocks)
        ]
    where
    genEnv :: BlockCount -> BlockTestMode (Maybe SlotId, OldestFirst NE Block)
    genEnv bCount = do
        initNodeDBs coreConfig
        g <- liftIO $ newStdGen
        bs <- flip evalRandT g $ genBlocks coreConfig (_tpTxpConfiguration tp)
                (BlockGenParams
                    { _bgpSecrets = mkAllSecretsSimple secretKeys
                    , _bgpBlockCount = bCount
                    , _bgpTxGenParams = TxGenParams
                        { _tgpTxCountRange = (0, 2)
                        , _tgpMaxOutputs = 2
                        }
                    , _bgpInplaceDB = False
                    , _bgpSkipNoKey = True -- TODO: should be False?
                    , _bgpGenStakeholders = configBootStakeholders coreConfig
                    , _bgpTxpGlobalSettings = txpGlobalSettings
                          coreConfig
                          (_tpTxpConfiguration tp)
                    })
                (maybeToList . fmap fst)
        let curSlot :: Maybe SlotId
            curSlot = case mapMaybe (either (const Nothing) Just . unEpochOrSlot . getEpochOrSlot) bs of
                [] -> Nothing
                ss -> Just $ maximum ss
        return $ (curSlot, OldestFirst $ NE.fromList bs)

    verifyAndApplyBlocksB
        :: Maybe SlotId
        -> OldestFirst NE Block
        -> Benchmarkable
    verifyAndApplyBlocksB curSlot blocks =
        nfIO
            $ runBTM tp ctx
            $ satisfySlotCheck blocks
            $ verifyAndApplyBlocks coreConfig (_tpTxpConfiguration tp) curSlot False blocks >>= \case
                    Left err -> return (Just err)
                    Right (_, blunds) -> do
                        whenJust (nonEmptyNewestFirst blunds)
                                 (rollbackBlocks coreConfig)
                        return Nothing

    verifyBlocksPrefixB
        :: Maybe SlotId
        -> OldestFirst NE Block
        -> Benchmarkable
    verifyBlocksPrefixB curSlot blocks =
        nfIO
            $ runBTM tp ctx
            $ satisfySlotCheck blocks
            $ map fst <$> verifyBlocksPrefix coreConfig curSlot blocks

-- | Benchmark which runs `verifyHeader`
verifyHeaderBenchmark
    :: HasConfigurations
    => Core.Config
    -> [SecretKey]
    -> TestParams
    -> Benchmark
verifyHeaderBenchmark !coreConfig !secretKeys !tp = env (runBlockTestMode coreConfig tp genEnv)
        $ \ ~(block, params) -> bgroup "block verification"
            [ bench "verifyHeader" $ benchHeaderVerification
                (getBlockHeader block, vbpVerifyHeader params)
            , bench "verifyBlock" $ benchBlockVerification
                (block, params)
            ]

    where
    pm = configProtocolMagic coreConfig
    genEnv :: BlockTestMode (Block, VerifyBlockParams)
    genEnv = do
        initNodeDBs coreConfig
        g <- liftIO $ newStdGen
        eos <- getEpochOrSlot <$> getTipHeader
        let epoch = eos ^. epochIndexL
        let blockGenParams = BlockGenParams
                { _bgpSecrets = mkAllSecretsSimple secretKeys
                , _bgpBlockCount = BlockCount 1
                , _bgpTxGenParams = TxGenParams
                    { _tgpTxCountRange = (0, 2)
                    , _tgpMaxOutputs = 2
                    }
                , _bgpInplaceDB = False
                , _bgpSkipNoKey = True -- TODO: should be False?
                , _bgpGenStakeholders = configBootStakeholders coreConfig
                , _bgpTxpGlobalSettings =
                      txpGlobalSettings coreConfig (_tpTxpConfiguration tp)
                }
        leaders <- lrcActionOnEpochReason epoch "genBlock" getLeadersForEpoch
        mblock <- flip evalRandT g $ do
            blockGenCtx <- lift $ mkBlockGenContext
                (configEpochSlots coreConfig)
                blockGenParams
            tipHeader <- lift $ getTipHeader
            mapRandT (flip runReaderT blockGenCtx)
                $ genBlockNoApply coreConfig (_tpTxpConfiguration tp) eos tipHeader
        let !block = fromMaybe (error "verifyHeaderBench: failed to generate a header") mblock

        let !verifyHeaderParams = VerifyHeaderParams
                { vhpPrevHeader = Nothing
                , vhpCurrentSlot = Nothing
                , vhpLeaders = Just leaders
                , vhpMaxSize = Nothing
                , vhpVerifyNoUnknown = False
                }
        let !params = VerifyBlockParams
                { vbpVerifyHeader = verifyHeaderParams
                , vbpMaxSize = bvdMaxBlockSize (_tpBlockVersionData tp)
                , vbpVerifyNoUnknown = False
                }

        return (block, params)

    benchHeaderVerification ~(header, params) =
        nf isVerSuccess $ verifyHeader pm params header

    benchBlockVerification ~(block, params) =
        nf isVerSuccess $ verifyBlock coreConfig params block


runBenchmark :: IO ()
runBenchmark = do
    startTime <- realTime
    let cfo = defaultConfigurationOptions
            { cfoFilePath = "../lib/configuration.yaml"
            , cfoKey = "bench-validation"
            , cfoSystemStart = Just (Timestamp startTime)
            }
    withCompileInfo
        $ withConfigurationsM (LoggerName "verifyBenchmark") Nothing Nothing False cfo
        $ \coreConfig txpConfig _ -> do
            let tp = TestParams
                    { _tpStartTime = Timestamp (convertUnit startTime)
                    , _tpBlockVersionData = configBlockVersionData coreConfig
                    , _tpGenesisInitializer = genesisInitializer
                    , _tpTxpConfiguration = txpConfig
                    }
            secretKeys <- gsSecretKeys <$> configGeneratedSecretsThrow coreConfig
            runEmulation startTime
                $ initBlockTestContext coreConfig tp $ \ctx ->
                    sudoLiftIO $ defaultMainWith config
                        [ verifyBlocksBenchmark coreConfig secretKeys tp ctx
                        , verifyHeaderBenchmark coreConfig secretKeys tp
                        ]
