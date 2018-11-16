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
import           Pos.Chain.Genesis as Genesis (Config (..),
                     FakeAvvmOptions (..), GenesisInitializer (..),
                     TestnetBalanceOptions (..), configBlockVersionData,
                     configBootStakeholders, configEpochSlots,
                     configGeneratedSecretsThrow, gsSecretKeys)
import           Pos.Chain.Update (BlockVersionData (..), ConsensusEra (..),
                     updateConfiguration)
import           Pos.Core.Chrono (NE, OldestFirst (..), nonEmptyNewestFirst)
import           Pos.Core.Common (BlockCount (..), unsafeCoinPortionFromDouble)
import           Pos.Core.NetworkMagic (makeNetworkMagic)
import           Pos.Core.Slotting (EpochOrSlot (..), SlotId, Timestamp (..),
                     epochIndexL, getEpochOrSlot)
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
                     withConfigurations)
import           Pos.Util.CompileInfo (withCompileInfo)
import           Pos.Util.Log.LoggerConfig (defaultInteractiveConfiguration)
import           Pos.Util.Util (realTime)
import           Pos.Util.Wlog (Severity (Debug), removeAllHandlers,
                     setupLogging')

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
    => Genesis.Config
    -> [SecretKey]
    -> TestParams
    -> BlockTestContext
    -> Benchmark
verifyBlocksBenchmark !genesisConfig !secretKeys !tp !ctx =
    bgroup "block verification"
        [ env (runBlockTestMode updateConfiguration genesisConfig tp (genEnv (BlockCount 100)))
            $ \ ~(curSlot, blocks) -> bench "verifyAndApplyBlocks" (verifyAndApplyBlocksB curSlot blocks)
        -- `verifyBlocksPrefix` will succeed only on the first block, it
        -- requires that blocks are applied.
        , env (runBlockTestMode updateConfiguration genesisConfig tp (genEnv (BlockCount 1)))
            $ \ ~(curSlot, blocks) -> bench "verifyBlocksPrefix" (verifyBlocksPrefixB curSlot blocks)
        ]
    where
    nm = makeNetworkMagic $ configProtocolMagic genesisConfig
    genEnv :: BlockCount -> BlockTestMode (Maybe SlotId, OldestFirst NE Block)
    genEnv bCount = do
        initNodeDBs genesisConfig
        g <- liftIO $ newStdGen
        bs <- flip evalRandT g $ genBlocks genesisConfig (_tpTxpConfiguration tp)
                (BlockGenParams
                    { _bgpSecrets = mkAllSecretsSimple nm secretKeys
                    , _bgpBlockCount = bCount
                    , _bgpTxGenParams = TxGenParams
                        { _tgpTxCountRange = (0, 2)
                        , _tgpMaxOutputs = 2
                        }
                    , _bgpInplaceDB = False
                    , _bgpSkipNoKey = True -- TODO: should be False?
                    , _bgpGenStakeholders = configBootStakeholders genesisConfig
                    , _bgpTxpGlobalSettings = txpGlobalSettings
                          genesisConfig
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
            $ verifyAndApplyBlocks genesisConfig (_tpTxpConfiguration tp) curSlot False blocks >>= \case
                    Left err -> return (Just err)
                    Right (_, blunds) -> do
                        whenJust (nonEmptyNewestFirst blunds)
                                 (rollbackBlocks genesisConfig)
                        return Nothing

    verifyBlocksPrefixB
        :: Maybe SlotId
        -> OldestFirst NE Block
        -> Benchmarkable
    verifyBlocksPrefixB curSlot blocks =
        nfIO
            $ runBTM tp ctx
            $ satisfySlotCheck blocks
            $ map fst <$> verifyBlocksPrefix genesisConfig curSlot blocks

-- | Benchmark which runs `verifyHeader`
verifyHeaderBenchmark
    :: HasConfigurations
    => Genesis.Config
    -> [SecretKey]
    -> TestParams
    -> Benchmark
verifyHeaderBenchmark !genesisConfig !secretKeys !tp =
    env (runBlockTestMode updateConfiguration genesisConfig tp genEnv)
        $ \ ~(block, params) -> bgroup "block verification"
            [ bench "verifyHeader" $ benchHeaderVerification
                (getBlockHeader block, vbpVerifyHeader params)
            , bench "verifyBlock" $ benchBlockVerification
                (block, params)
            ]

    where
    pm = configProtocolMagic genesisConfig
    nm = makeNetworkMagic pm
    genEnv :: BlockTestMode (Block, VerifyBlockParams)
    genEnv = do
        initNodeDBs genesisConfig
        g <- liftIO $ newStdGen
        eos <- getEpochOrSlot <$> getTipHeader
        let epoch = eos ^. epochIndexL
        let blockGenParams = BlockGenParams
                { _bgpSecrets = mkAllSecretsSimple nm secretKeys
                , _bgpBlockCount = BlockCount 1
                , _bgpTxGenParams = TxGenParams
                    { _tgpTxCountRange = (0, 2)
                    , _tgpMaxOutputs = 2
                    }
                , _bgpInplaceDB = False
                , _bgpSkipNoKey = True -- TODO: should be False?
                , _bgpGenStakeholders = configBootStakeholders genesisConfig
                , _bgpTxpGlobalSettings =
                      txpGlobalSettings genesisConfig (_tpTxpConfiguration tp)
                }
        leaders <- lrcActionOnEpochReason epoch "genBlock" getLeadersForEpoch
        mblock <- flip evalRandT g $ do
            blockGenCtx <- lift $ mkBlockGenContext
                (configEpochSlots genesisConfig)
                blockGenParams
            tipHeader <- lift $ getTipHeader
            mapRandT (flip runReaderT blockGenCtx)
                $ genBlockNoApply genesisConfig (_tpTxpConfiguration tp) eos tipHeader
        let !block = fromMaybe (error "verifyHeaderBench: failed to generate a header") mblock

        let !verifyHeaderParams = VerifyHeaderParams
                { vhpPrevHeader = Nothing
                , vhpCurrentSlot = Nothing
                , vhpLeaders = Just leaders
                , vhpMaxSize = Nothing
                , vhpVerifyNoUnknown = False
                -- @intricate @mhuesch: hardcoded `Original` consensus era
                , vhpConsensusEra = Original
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
        -- @intricate @mhuesch: hardcoded `Original` consensus era verification
        nf isVerSuccess $ verifyBlock genesisConfig Original params block


runBenchmark :: IO ()
runBenchmark = do
    let loggerConfig = defaultInteractiveConfiguration Debug
    lh <- setupLogging' "verifyBenchmark" loggerConfig
    startTime <- realTime
    let cfo = defaultConfigurationOptions
            { cfoFilePath = "../lib/configuration.yaml"
            , cfoKey = "bench-validation"
            , cfoSystemStart = Just (Timestamp startTime)
            }
    withCompileInfo
        $ withConfigurations Nothing Nothing False cfo
        $ \genesisConfig _ txpConfig _ -> do
            let tp = TestParams
                    { _tpStartTime = Timestamp (convertUnit startTime)
                    , _tpBlockVersionData = configBlockVersionData genesisConfig
                    , _tpGenesisInitializer = genesisInitializer
                    , _tpTxpConfiguration = txpConfig
                    , _tpProtocolMagic = configProtocolMagic genesisConfig
                    }
            secretKeys <- gsSecretKeys <$> configGeneratedSecretsThrow genesisConfig
            runEmulation startTime
                $ initBlockTestContext updateConfiguration genesisConfig tp $ \ctx ->
                    sudoLiftIO $ defaultMainWith config
                        [ verifyBlocksBenchmark genesisConfig secretKeys tp ctx
                        , verifyHeaderBenchmark genesisConfig secretKeys tp
                        ]
    removeAllHandlers lh
