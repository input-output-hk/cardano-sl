module Bench.Pos.Criterion.Block.Logic
    ( runBenchmark
    ) where

import           Universum

import           Control.Monad.Random.Strict (evalRandT, mapRandT)
import           Criterion.Main (Benchmark, Benchmarkable, bench, bgroup, defaultConfig, defaultMainWith, env, nf, nfIO)
import           Criterion.Types (Config (..), Verbosity (..))
import qualified Data.List.NonEmpty as NE
import           Data.Time.Units (convertUnit)
import           System.Random (newStdGen)
import           System.Wlog (LoggerName (..))
import           Serokell.Util.Verify (isVerSuccess)

import           Mockable.CurrentTime (realTime)

import           Pos.AllSecrets (mkAllSecretsSimple)
import           Pos.Block.Logic.Integrity (VerifyBlockParams (..),
                     VerifyHeaderParams (..), verifyBlock, verifyHeader)
import           Pos.Block.Logic.VAR (VerifyBlocksContext,
                     getVerifyBlocksContext', rollbackBlocks,
                     verifyAndApplyBlocks, verifyBlocksPrefix)
import           Pos.Core (Block, EpochOrSlot (..), SlotId, getBlockHeader)
import           Pos.Core.Chrono (NE, OldestFirst (..), nonEmptyNewestFirst)
import           Pos.Core.Common (BlockCount (..), unsafeCoinPortionFromDouble)
import           Pos.Core.Configuration (genesisBlockVersionData, genesisData, genesisSecretKeys, slotSecurityParam)
import           Pos.Core.Genesis (FakeAvvmOptions (..), GenesisData (..), GenesisInitializer (..), TestnetBalanceOptions (..))
import           Pos.Core.Slotting (Timestamp (..))
import           Pos.Core.Update (BlockVersionData (..))
import           Pos.Crypto.Configuration (ProtocolMagic (..))
import           Pos.DB.DB (initNodeDBs)
import           Pos.DB (getTipHeader)
import           Pos.Generator.Block (BlockGenParams (..), TxGenParams (..),
                     genBlockNoApply, genBlocks, mkBlockGenContext)
import           Pos.Launcher.Configuration (ConfigurationOptions (..),
                     HasConfigurations, defaultConfigurationOptions,
                     withConfigurationsM)
import           Pos.Lrc.Context (lrcActionOnEpochReason)
import qualified Pos.Lrc.DB as LrcDB
import           Pos.Txp.Logic.Global (txpGlobalSettings)
import           Pos.Util.CompileInfo (withCompileInfo)
import           Test.Pos.Block.Logic.Emulation (runEmulation, sudoLiftIO)
import           Test.Pos.Block.Logic.Mode (BlockTestContext, BlockTestMode,
                     TestParams (..), initBlockTestContext, runBlockTestMode)
import           Test.Pos.Block.Logic.Util (satisfySlotCheck)

-- | Criterion configuration
config :: Config
config = defaultConfig
    { reportFile = Just "verification.html"
    , resamples = 10
    , timeLimit = 5.0
    , verbosity = Verbose
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
    => ProtocolMagic
    -> TestParams
    -> BlockTestContext
    -> Benchmark
verifyBlocksBenchmark !pm !tp !ctx =
    bgroup "block verification"
        [ env (runBlockTestMode tp (genEnv (BlockCount 100)))
            $ \e -> bench "verifyAndApplyBlocks" (verifyAndApplyBlocksB e)
        -- `verifyBlocksPrefix` will succeed only on the first block, it
        -- requires that blocks are applied.
        , env (runBlockTestMode tp (genEnv (BlockCount 1)))
            $ \e -> bench "verifyBlocksPrefix" (verifyBlocksPrefixB e)
        ]
    where
    genEnv :: BlockCount -> BlockTestMode (OldestFirst NE Block)
    genEnv bCount = do
        initNodeDBs pm slotSecurityParam
        g <- liftIO $ newStdGen
        let secretKeys = case genesisSecretKeys of
                Nothing -> error "verifyAndApplyBlocksBenchmark: no genesisSecretKeys"
                Just ks -> ks
        bs <- flip evalRandT g $ genBlocks pm
                (BlockGenParams
                    { _bgpSecrets = mkAllSecretsSimple secretKeys
                    , _bgpBlockCount = bCount
                    , _bgpTxGenParams = TxGenParams
                        { _tgpTxCountRange = (0, 2)
                        , _tgpMaxOutputs = 2
                        }
                    , _bgpInplaceDB = False
                    , _bgpSkipNoKey = True -- TODO: should be False?
                    , _bgpGenStakeholders = gdBootStakeholders genesisData
                    , _bgpTxpGlobalSettings = txpGlobalSettings pm
                    })
                (maybeToList . fmap fst)
        return $ (OldestFirst $ NE.fromList bs)

    verifyAndApplyBlocksB
        :: OldestFirst NE Block
        -> Benchmarkable
    verifyAndApplyBlocksB blocks =
        nfIO
            $ runBTM tp ctx
            $ satisfySlotCheck blocks
            $ verifyAndApplyBlocks pm False blocks >>= \case
                    Left err -> return (Just err)
                    Right (_, blunds) -> do
                        whenJust (nonEmptyNewestFirst blunds) (rollbackBlocks pm)
                        return Nothing

    verifyBlocksPrefixB
        :: OldestFirst NE Block
        -> Benchmarkable
    verifyBlocksPrefixB blocks =
        nfIO
            $ runBTM tp ctx
            $ satisfySlotCheck blocks
            $ map fst <$> verifyBlocksPrefix pm blocks

-- | Benchmark which runs `verifyHeader`
verifyHeaderBenchmark
    :: HasConfigurations
    => ProtocolMagic
    -> TestParams
    -> Benchmark
verifyHeaderBenchmark !pm !tp = env (runBlockTestMode tp genEnv)
        $ \ ~(block, params) -> bgroup "block verification"
            [ bench "verifyHeader" $ benchHeaderVerification
                (getBlockHeader block, vbpVerifyHeader params)
            , bench "verifyBlock" $ benchBlockVerification
                (block, params)
            ]

    where
    genEnv :: BlockTestMode (Block, VerifyBlockParams)
    genEnv = do
        initNodeDBs pm slotSecurityParam
        g <- liftIO $ newStdGen
        eos <- getEpochOrSlot <$> getTipHeader
        let epoch = eos ^. epochIndexL
        let secretKeys = case genesisSecretKeys of
                Nothing -> error "verifyHeaderBench: no genesisSecretKeys"
                Just ks -> ks
        let blockGenParams = BlockGenParams
                { _bgpSecrets = mkAllSecretsSimple secretKeys
                , _bgpBlockCount = BlockCount 1
                , _bgpTxGenParams = TxGenParams
                    { _tgpTxCountRange = (0, 2)
                    , _tgpMaxOutputs = 2
                    }
                , _bgpInplaceDB = False
                , _bgpSkipNoKey = True -- TODO: should be False?
                , _bgpGenStakeholders = gdBootStakeholders genesisData
                , _bgpTxpGlobalSettings = txpGlobalSettings pm
                }
        leaders <- lrcActionOnEpochReason epoch "genBlock" LrcDB.getLeadersForEpoch
        mblock <- flip evalRandT g $ do
            blockGenCtx <- lift $ mkBlockGenContext blockGenParams
            tipHeader <- lift $ getTipHeader
            mapRandT (flip runReaderT blockGenCtx)
                $ genBlockNoApply pm eos tipHeader
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
        nf isVerSuccess $ verifyBlock pm params block


runBenchmark :: IO ()
runBenchmark = do
    startTime <- realTime
    let cfo = defaultConfigurationOptions
            { cfoFilePath = "../lib/configuration.yaml"
            , cfoKey = "bench-validation"
            , cfoSystemStart = Just (Timestamp startTime)
            }
    withCompileInfo $
        withConfigurationsM (LoggerName "verifyBenchmark") Nothing cfo $ \_ pm -> do
                let tp = TestParams
                        { _tpStartTime = Timestamp (convertUnit startTime)
                        , _tpBlockVersionData = genesisBlockVersionData
                        , _tpGenesisInitializer = genesisInitializer
                        }
                runEmulation startTime
                    $ initBlockTestContext tp $ \ctx ->
                        sudoLiftIO $ defaultMainWith config
                            [ verifyBlocksBenchmark pm tp ctx
                            , verifyHeaderBenchmark pm tp
                            ]
