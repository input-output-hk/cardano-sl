{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main (main) where

import qualified Data.ByteString as BS
import           Data.Constraint (Dict (Dict))
import           Data.Default (def)
import           Data.Ix (range)
import qualified Data.Text as T
import           System.Environment (getEnv)
import           System.Exit (ExitCode (..))
import           System.IO (hPutStrLn)
import           Universum hiding (on)

import           Pos.Chain.Genesis (GenesisSpec (..), StaticConfig (..))
import           Pos.Chain.Update (ApplicationName (ApplicationName),
                     BlockVersion (BlockVersion),
                     BlockVersionData (bvdMaxBlockSize, bvdUnlockStakeEpoch),
                     BlockVersionModifier (bvmMaxBlockSize, bvmUnlockStakeEpoch),
                     SoftwareVersion (..), UpdateConfiguration,
                     ccApplicationName_L, ccApplicationVersion_L,
                     ccLastKnownBlockVersion_L, obftEraFlagValue)
import qualified Pos.Client.CLI as CLI
import           Pos.Core.Slotting (EpochIndex)
import           Pos.DB.Class (gsAdoptedBVData)
import qualified Pos.GState as GS
import           Pos.Infra.Diffusion.Types (Diffusion)
import           Pos.Launcher (Configuration, HasConfigurations, ccGenesis_L,
                     ccUpdate_L, cfoFilePath_L, cfoKey_L)
import           Pos.Util.Util (lensOf)

import           AutomatedTestRunner
import           BlockParser ()
import           NodeControl (NodeInfo (..), mutateConfigurationYaml, startNode,
                     stopNodeByName)
import           OrphanedLenses ()
import           PocMode
import           Types (NodeType (..))

import           Serokell.Data.Memory.Units (Byte)

-- | Helper to make sure BV and SV are set in accordance with the arguments passed in
confSetBvAndSv :: BlockVersion -> SoftwareVersion -> Configuration -> Configuration
confSetBvAndSv bv sv cfg = ((cfg & ccUpdate_L . ccLastKnownBlockVersion_L .~ bv)
                                 & ccUpdate_L . ccApplicationVersion_L .~ (svNumber sv))
                                 & ccUpdate_L . ccApplicationName_L .~ (svAppName sv)

mutateConfigurationForObft :: Configuration -> Configuration
mutateConfigurationForObft = over ccGenesis_L (updateUnlockStakeEpoch obftEraFlagValue)
 where
  updateUnlockStakeEpoch :: EpochIndex -> StaticConfig -> StaticConfig
  updateUnlockStakeEpoch _  (GCSrc _ _) = error "updateUnlockStakeEpoch: got GCSrc"
  updateUnlockStakeEpoch ei (GCSpec gs) = GCSpec (gs { gsBlockVersionData = ((gsBlockVersionData gs) { bvdUnlockStakeEpoch = ei })})

data ExpectedResult = SuccessFullUpdate | FailedProposalUpdate deriving (Show, Eq)

-- most logging to console is disabled to reduce signal to noise ratio
-- but the messages from this script should always go to the console
-- maybe use a different log priority config to handle things better?
logMsg :: String -> PocMode ()
logMsg = liftIO . hPutStrLn stderr

sharedUpdateTester :: (Configuration -> Configuration)
                   -> ExpectedResult
                   -> BlockVersion
                   -> SoftwareVersion
                   -> BlockVersionModifier
                   -> (BlockVersionData -> Bool)
                   -> String
                   -> Script ()
sharedUpdateTester mutateConf expectedResult blockVersion softwareVersion
                   blockVersionModifier checkBvd errMsg = do
  genesisConfig <- getGenesisConfig
  let
    proposal :: Dict HasConfigurations -> Diffusion PocMode -> PocMode ()
    proposal Dict diffusion = do
      let
        keyIndex :: Int
        keyIndex = 0
      doUpdate diffusion genesisConfig keyIndex blockVersion softwareVersion blockVersionModifier
  onStartup $ \Dict _diffusion -> do
    stateDir <- view acStatePath
    loadNKeys stateDir =<< getNodeCount
  on (1,2) proposal
  on (1,6) $ \Dict _diffusion -> do
    uc <- view (lensOf @UpdateConfiguration)
    proposals <- GS.getConfirmedProposals uc Nothing
    case (proposals, expectedResult) of
      ([], FailedProposalUpdate) -> do
        logMsg "test passed: proposal failed, as expected"
        endScript ExitSuccess
      (_, FailedProposalUpdate) -> do
        logMsg "test failed: proposal was accepted, but we expected failure"
        endScript $ ExitFailure 1
      ([], _) -> do
        logMsg "test failed: proposal failed, but we expected success"
        endScript $ ExitFailure 2
      ([_one], SuccessFullUpdate) -> do
        stateDir <- view acStatePath
        opts <- view acScriptOptions
        let
          -- the config for the script-runner is mirrored to the nodes it starts
          cfg = opts ^. srCommonNodeArgs . CLI.commonArgs_L
          fullConfMutator = mutateConf . confSetBvAndSv blockVersion softwareVersion
          --
        newConfiguration <- liftIO $ mutateConfigurationYaml (cfg ^. CLI.configurationOptions_L . cfoFilePath_L)
                                                             (cfg ^. CLI.configurationOptions_L . cfoKey_L)
                                                             fullConfMutator
        liftIO $ BS.writeFile (T.unpack $ stateDir <> "/configuration2.yaml") newConfiguration
        let
          cfg2 = cfg & CLI.configurationOptions_L . cfoFilePath_L .~ (T.unpack $ stateDir <> "/configuration2.yaml")
        forAllNodes_ $ \node -> do
          stopNodeByName (Core, node)
          startNode $ NodeInfo node Core stateDir (stateDir <> "/topology.yaml") cfg2
      (_toomany, SuccessFullUpdate) -> do
        logMsg "test failed: expected 1 proposal to pass, but >1 have passed"
        endScript $ ExitFailure 3
  on (3,10) $ \Dict _diffusion -> do
    bvd <- gsAdoptedBVData
    if checkBvd bvd
       then do
         liftIO $ hPutStrLn stderr "test passed"
         endScript ExitSuccess
       else do
         liftIO $ hPutStrLn stderr errMsg
         endScript $ ExitFailure 4
  forM_ (range (0,20)) $ \epoch -> do
    on(epoch, 0) $ printbvd epoch 0
    on(epoch, 1) $ printbvd epoch 1

testHardForkMechanism :: Script ()
testHardForkMechanism =
  sharedUpdateTester mutateConf expectedResult blockVersion softwareVersion
                     blockVersionModifier checkBvd errMsg
 where
  mutateConf :: Configuration -> Configuration
  mutateConf = mutateConfigurationForObft
  expectedResult :: ExpectedResult
  expectedResult = SuccessFullUpdate
  blockVersion :: BlockVersion
  blockVersion = BlockVersion 1 0 0
  softwareVersion :: SoftwareVersion
  softwareVersion = SoftwareVersion (ApplicationName "cardano-sl") 0
  blockVersionModifier :: BlockVersionModifier
  blockVersionModifier = def { bvmUnlockStakeEpoch = Just obftEraFlagValue }
  checkBvd :: BlockVersionData -> Bool
  checkBvd bvd = bvdUnlockStakeEpoch bvd == obftEraFlagValue
  errMsg :: String
  errMsg = "bvdUnlockStakeEpoch was not what was we expected"

test4 :: Byte -> ExpectedResult -> Script ()
test4 targetblocksize expectedResult =
  sharedUpdateTester mutateConf expectedResult blockVersion softwareVersion
                     blockVersionModifier checkBvd errMsg
 where
  mutateConf :: Configuration -> Configuration
  mutateConf = identity
  blockVersion :: BlockVersion
  blockVersion = BlockVersion 0 1 0
  softwareVersion :: SoftwareVersion
  softwareVersion = SoftwareVersion (ApplicationName "cardano-sl") 1
  blockVersionModifier :: BlockVersionModifier
  blockVersionModifier = def { bvmMaxBlockSize = Just targetblocksize }
  checkBvd :: BlockVersionData -> Bool
  checkBvd bvd = bvdMaxBlockSize bvd == targetblocksize
  errMsg :: String
  errMsg = "max block size not what was expected"

emptyScript :: Script ()
emptyScript = do
  pure ()

main :: IO ()
main = do
  scriptArg <- getEnv "SCRIPT"
  let
    scripts :: [(String, Script ())]
    scripts = [ ("test4.1", test4 1000000 SuccessFullUpdate)
              , ("test4.2", test4 10000000 FailedProposalUpdate)
              , ("testHFM", testHardForkMechanism)
              , ("none"   , emptyScript)
              ]

    getAutoMode :: String -> Bool
    getAutoMode "none" = False
    getAutoMode _      = True

  script <- case find ((scriptArg==) . fst) scripts of
               Nothing -> do
                 putStrLn ("\ninvalid script: " <> show scriptArg
                        <> "\navailable options are " <> show (map fst scripts)
                        <> "\n" :: Text)
                 exitFailure
               Just (_name,sc) -> pure sc

  runScript $ ScriptParams
    { spScript = script
    , spRecentSystemStart = getAutoMode scriptArg
    , spStartCoreAndRelay = getAutoMode scriptArg
    }
