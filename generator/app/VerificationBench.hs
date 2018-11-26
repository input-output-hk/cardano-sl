module VerificationBench where

import           Universum

import           Control.DeepSeq (force)
import           Control.Monad.Random.Strict (evalRandT)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Time.Units (Microsecond, convertUnit)
import           Formatting (int, sformat, shown, (%))
import qualified Options.Applicative as Opts
import           System.Directory (doesFileExist)
import           System.Random (newStdGen)

import           Pos.AllSecrets (mkAllSecretsSimple)
import           Pos.Binary.Class (decodeFull, serialize)
import           Pos.Chain.Block (ApplyBlocksException, Block,
                     VerifyBlocksException)
import           Pos.Chain.Genesis as Genesis (Config (..),
                     configBlockVersionData, configBootStakeholders,
                     configGeneratedSecretsThrow)
import           Pos.Chain.Genesis (FakeAvvmOptions (..),
                     GenesisInitializer (..), TestnetBalanceOptions (..),
                     gsSecretKeys)
import           Pos.Chain.Txp (TxpConfiguration (..))
import           Pos.Chain.Update (updateConfiguration)
import           Pos.Core (ProtocolConstants (..))
import           Pos.Core.Chrono (NE, OldestFirst (..), nonEmptyNewestFirst)
import           Pos.Core.Common (BlockCount (..), unsafeCoinPortionFromDouble)
import           Pos.Core.NetworkMagic (makeNetworkMagic)
import           Pos.Core.Slotting (Timestamp (..))
import           Pos.Crypto (SecretKey)
import           Pos.DB.Block (rollbackBlocks, verifyAndApplyBlocks,
                     verifyBlocksPrefix)
import           Pos.DB.DB (initNodeDBs)
import           Pos.DB.Txp.Logic (txpGlobalSettings)
import           Pos.Generator.Block (BlockGenParams (..), TxGenParams (..),
                     genBlocks)
import           Pos.Launcher.Configuration (ConfigurationOptions (..),
                     HasConfigurations, defaultConfigurationOptions,
                     withConfigurations)
import           Pos.Util.CompileInfo (withCompileInfo)
import           Pos.Util.Log.LoggerConfig (defaultInteractiveConfiguration)
import           Pos.Util.Util (realTime)
import           Pos.Util.Wlog (LoggerConfig, Severity (Debug), logError,
                     logInfo, removeAllHandlers, setupLogging')

import           Test.Pos.Block.Logic.Mode (BlockTestMode, TestParams (..),
                     runBlockTestMode)
import           Test.Pos.Block.Logic.Util (satisfySlotCheck)

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

generateBlocks :: HasConfigurations
               => Genesis.Config
               -> [SecretKey]
               -> TxpConfiguration
               -> BlockCount
               -> BlockTestMode (OldestFirst NE Block)
generateBlocks genesisConfig secretKeys txpConfig bCount = do
    g <- liftIO $ newStdGen
    let nm = makeNetworkMagic $ configProtocolMagic genesisConfig
    bs <- flip evalRandT g $ genBlocks genesisConfig txpConfig
            (BlockGenParams
                { _bgpSecrets = mkAllSecretsSimple nm secretKeys
                , _bgpBlockCount = bCount
                , _bgpTxGenParams = TxGenParams
                    { _tgpTxCountRange = (0, 2)
                    , _tgpMaxOutputs = 2
                    }
                , _bgpInplaceDB = False
                , _bgpSkipNoKey = True
                , _bgpGenStakeholders = configBootStakeholders genesisConfig
                , _bgpTxpGlobalSettings = txpGlobalSettings
                      genesisConfig
                      (TxpConfiguration 200 Set.empty)
                })
            (maybeToList . fmap fst)
    return $ OldestFirst $ NE.fromList bs


data BenchArgs = BenchArgs
    { baConfigPath :: FilePath
    , baConfigKey  :: Text
    , baBlockCount :: BlockCount
    , baRuns       :: Int
    , baApply      :: Bool
    , baBlockCache :: Maybe FilePath
    , baK          :: Int
    -- ^ the protocol constant k
    }

configPathP :: Opts.Parser FilePath
configPathP = Opts.strOption $
       Opts.long "config"
    <> Opts.value "lib/configuration.yaml"
    <> Opts.showDefault
    <> Opts.help "path to yaml configuration file"

configKeyP :: Opts.Parser String
configKeyP = Opts.strOption $
       Opts.long "config-key"
    <> Opts.value "test"
    <> Opts.showDefault
    <> Opts.help "configuration key"

blockCountP :: Opts.Parser BlockCount
blockCountP = Opts.option (BlockCount <$> Opts.auto) $
       Opts.long "block-count"
    <> Opts.value 2000
    <> Opts.showDefault
    <> Opts.help "number of blocks to generate"

runsP :: Opts.Parser Int
runsP = Opts.option Opts.auto $
       Opts.long "runs"
    <> Opts.short 'r'
    <> Opts.value 100
    <> Opts.showDefault
    <> Opts.help "number of runs over generated blocks"

applyBlocksP :: Opts.Parser Bool
applyBlocksP = Opts.switch $
       Opts.long "apply"
    <> Opts.short 'a'
    <> Opts.help "apply blocks: runs `verifyAndApplyBlocks` otherwise it runs `verifyBlocksPrefix`"

blockCacheP :: Opts.Parser (Maybe FilePath)
blockCacheP = Opts.optional $ Opts.strOption $
       Opts.long "block-cache"
    <> Opts.help "path to block cache (file where generated blocks are written / read from)"

kP :: Opts.Parser Int
kP = Opts.option Opts.auto $
       Opts.short 'k'
    <> Opts.value 5000
    <> Opts.showDefault
    <> Opts.help "the security parameter k"

benchArgsParser :: Opts.Parser BenchArgs
benchArgsParser = BenchArgs
    <$> configPathP
    <*> (T.pack <$> configKeyP)
    <*> blockCountP
    <*> runsP
    <*> applyBlocksP
    <*> blockCacheP
    <*> kP

-- | Write generated blocks to a file.
writeBlocks :: FilePath -> OldestFirst NE Block -> IO ()
writeBlocks path bs = do
    let sbs = serialize bs
    BSL.writeFile path sbs

-- | Read generated blocks from a file.
readBlocks :: FilePath -> IO (Maybe (OldestFirst NE Block))
readBlocks path = do
    sbs <- BSL.readFile path
    case decodeFull sbs of
        Left err -> do
            putStrLn err
            return Nothing
        Right bs -> return (Just bs)

main :: IO ()
main = do
    lh <- setupLogging' "verification-bench" loggerConfig
    args <- Opts.execParser
        $ Opts.info
            (benchArgsParser <**> Opts.helper)
            (Opts.fullDesc <> Opts.progDesc
                (  "The program generates given ammount of blocks and applies them. "
                )
            )
    startTime <- realTime
    let cfo = defaultConfigurationOptions
            { cfoFilePath = baConfigPath args
            , cfoKey = baConfigKey args
            , cfoSystemStart = Just (Timestamp startTime)
            }
    withCompileInfo $
        withConfigurations Nothing Nothing False cfo $ \ !genesisConfig !_ !txpConfig !_ -> do
            let genesisConfig' = genesisConfig
                    { configProtocolConstants =
                        (configProtocolConstants genesisConfig) { pcK = baK args }
                    }
                tp = TestParams
                    { _tpStartTime = Timestamp (convertUnit startTime)
                    , _tpBlockVersionData = configBlockVersionData genesisConfig'
                    , _tpGenesisInitializer = genesisInitializer
                    , _tpTxpConfiguration = TxpConfiguration 200 Set.empty
                    , _tpProtocolMagic = configProtocolMagic genesisConfig
                    }
            secretKeys <- gsSecretKeys <$> configGeneratedSecretsThrow genesisConfig'
            runBlockTestMode updateConfiguration genesisConfig' tp $ do
                -- initialize databasea
                initNodeDBs genesisConfig'
                bs <- case baBlockCache args of
                    Nothing -> do
                        -- generate blocks and evaluate them to normal form
                        logInfo "Generating blocks"
                        generateBlocks genesisConfig' secretKeys txpConfig (baBlockCount args)
                    Just path -> do
                        fileExists <- liftIO $ doesFileExist path
                        mbs <- if fileExists
                                  then liftIO $ readBlocks path
                                  else return Nothing
                        case mbs of
                            Nothing -> do
                                -- generate blocks and evaluate them to normal form
                                logInfo "Generating blocks"
                                bs <- generateBlocks genesisConfig' secretKeys txpConfig (baBlockCount args)
                                liftIO $ writeBlocks path bs
                                return bs
                            Just bs -> return bs

                satisfySlotCheck bs $ do
                    logInfo "Verifying blocks"
                    let bss = force $ zip ([1..] :: [Int]) $ replicate (baRuns args) bs
                    (times, errs) <- fmap unzip $ forM bss
                        $ \(idx, blocks) -> do
                            logInfo $ sformat ("Pass: "%int) idx
                            (if baApply args
                                then validateAndApply genesisConfig' txpConfig blocks
                                else validate genesisConfig' blocks)

                    let -- drop first three results (if there are more than three results)
                        itimes :: [Float]
                        itimes = map realToFrac (if length times > 3 then drop 3 times else times)
                        -- execution mean time
                        mean :: Float
                        mean = avarage itimes
                        -- standard deviation of the execution time distribution
                        stddev :: Float
                        stddev = sqrt . (\x -> x / realToFrac (length itimes - 1)) . avarage . map ((**2) . (-) mean) $ itimes
                    logInfo $ sformat ("verification and application mean time: "%shown%"μs stddev: "%shown) mean stddev

                    -- print errors
                    let errs' = catMaybes errs
                        errno = length errs'
                    when (errno > 0) $ do
                        logError $ sformat ("Verification/Application errors ("%shown%"):") errno
                        traverse_ (logError . show) errs
    removeAllHandlers lh
    where
        loggerConfig :: LoggerConfig
        loggerConfig = defaultInteractiveConfiguration Debug

        avarage :: [Float] -> Float
        avarage as = sum as / realToFrac (length as)

        validate
            :: HasConfigurations
            => Genesis.Config
            -> OldestFirst NE Block
            -> BlockTestMode (Microsecond, Maybe (Either VerifyBlocksException ApplyBlocksException))
        validate genesisConfig blocks = do
            verStart <- realTime
            res <- (force . either Left (Right . fst)) <$> verifyBlocksPrefix genesisConfig Nothing blocks
            verEnd <- realTime
            return (verEnd - verStart, either (Just . Left) (const Nothing) res)

        validateAndApply
            :: HasConfigurations
            => Genesis.Config
            -> TxpConfiguration
            -> OldestFirst NE Block
            -> BlockTestMode (Microsecond, Maybe (Either VerifyBlocksException ApplyBlocksException))
        validateAndApply genesisConfig txpConfig blocks = do
            verStart <- realTime
            res <- force <$> verifyAndApplyBlocks genesisConfig txpConfig Nothing False blocks
            verEnd <- realTime
            case res of
                Left _ -> return ()
                Right (_, blunds) -> whenJust
                    (nonEmptyNewestFirst blunds)
                    (rollbackBlocks genesisConfig)
            return (verEnd - verStart, either (Just . Right) (const Nothing) res)
