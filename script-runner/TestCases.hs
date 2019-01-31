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

import           Pos.Chain.Update (ApplicationName (ApplicationName),
                     BlockVersion (BlockVersion),
                     BlockVersionData (bvdMaxBlockSize),
                     BlockVersionModifier (bvmMaxBlockSize),
                     SoftwareVersion (SoftwareVersion), UpdateConfiguration,
                     ccApplicationVersion_L, ccLastKnownBlockVersion_L)
import qualified Pos.Client.CLI as CLI
import           Pos.DB.Class (gsAdoptedBVData)
import qualified Pos.GState as GS
import           Pos.Infra.Diffusion.Types (Diffusion)
import           Pos.Launcher (Configuration, HasConfigurations, ccUpdate_L,
                     cfoFilePath_L, cfoKey_L)
import           Pos.Util.Util (lensOf)

import           AutomatedTestRunner
import           BlockParser ()
import           NodeControl (NodeInfo (..), mutateConfigurationYaml, startNode,
                     stopNodeByName)
import           OrphanedLenses ()
import           PocMode
import           Types (NodeType (..), Todo (Todo))

import           Serokell.Data.Memory.Units (Byte)

mutateConfiguration :: Configuration -> Configuration
mutateConfiguration cfg = (cfg & ccUpdate_L . ccLastKnownBlockVersion_L .~ BlockVersion 0 1 0) & ccUpdate_L . ccApplicationVersion_L .~ 1

data ExpectedResult = SuccessFullUpdate | FailedProposalUpdate deriving (Show, Eq)

-- most logging to console is disabled to reduce signal to noise ratio
-- but the messages from this script should always go to the console
-- maybe use a different log priority config to handle things better?
logMsg :: String -> PocMode ()
logMsg = liftIO . hPutStrLn stderr

test4 :: Byte -> ExpectedResult -> Script ()
test4 targetblocksize expectedResult = do
  genesisConfig <- getGenesisConfig
  let
    proposal :: Dict HasConfigurations -> Diffusion PocMode -> PocMode ()
    proposal Dict diffusion = do
      let
        keyIndex :: Int
        keyIndex = 0
        blockVersion = BlockVersion 0 1 0
        softwareVersion = SoftwareVersion (ApplicationName "cardano-sl") 1
        blockVersionModifier :: BlockVersionModifier
        blockVersionModifier = def { bvmMaxBlockSize = Just targetblocksize }
      doUpdate diffusion genesisConfig keyIndex blockVersion softwareVersion blockVersionModifier
  onStartup $ \Dict _diffusion -> do
    stateDir <- view acStatePath
    loadNKeys stateDir 4
  on (1,2) proposal
  on (1,6) $ \Dict _diffusion -> do
    uc <- view (lensOf @UpdateConfiguration)
    proposals <- GS.getConfirmedProposals uc Nothing
    case (proposals, expectedResult) of
      ([], FailedProposalUpdate) -> do
        logMsg "expected failed proposal, passing test"
        endScript ExitSuccess
      (_, FailedProposalUpdate) -> do
        logMsg "expected failure, but proposal was accepted!"
        endScript $ ExitFailure 1
      ([], _) -> do
        logMsg "expected proposal to pass, but it didnt"
        endScript $ ExitFailure 2
      ([_one], SuccessFullUpdate) -> do
        stateDir <- view acStatePath
        opts <- view acScriptOptions
        let
          -- the config for the script-runner is mirrored to the nodes it starts
          cfg = opts ^. srCommonNodeArgs . CLI.commonArgs_L
        newConfiguration <- liftIO $ mutateConfigurationYaml (cfg ^. CLI.configurationOptions_L . cfoFilePath_L) (cfg ^. CLI.configurationOptions_L . cfoKey_L) mutateConfiguration
        liftIO $ BS.writeFile (T.unpack $ stateDir <> "/configuration2.yaml") newConfiguration
        let
          cfg2 = cfg & CLI.configurationOptions_L . cfoFilePath_L .~ (T.unpack $ stateDir <> "/configuration2.yaml")
        forM_ (range (0,3)) $ \node -> do
          stopNodeByName (Core, node)
          startNode $ NodeInfo node Core stateDir (stateDir <> "/topology.yaml") cfg2
      (_toomany, SuccessFullUpdate) -> do
        logMsg "expected 1 proposal to pass, but >1 have passed"
        endScript $ ExitFailure 3
  on (3,10) $ \Dict _diffusion -> do
    bvd <- gsAdoptedBVData
    case (bvdMaxBlockSize bvd == targetblocksize) of
      True -> endScript ExitSuccess
      _ -> do
        liftIO $ hPutStrLn stderr "max block size not what was expected"
        endScript $ ExitFailure 4
  forM_ (range (0,20)) $ \epoch -> do
    on(epoch, 0) $ printbvd epoch 0
    on(epoch, 1) $ printbvd epoch 1

emptyScript :: Script ()
emptyScript = do
  pure ()

main :: IO ()
main = do
  script <- getEnv "SCRIPT"
  let
    getScript :: String -> Script ()
    getScript "test4.1" = test4 1000000 SuccessFullUpdate
    getScript "test4.2" = test4 10000000 FailedProposalUpdate
    getScript "none"    = emptyScript
    getAutoMode :: String -> Bool
    getAutoMode "none" = False
    getAutoMode _      = True
  runScript $ ScriptParams
    { spTodo = (Todo 4)
    , spScript = getScript script
    , spRecentSystemStart = getAutoMode script
    , spStartCoreAndRelay = getAutoMode script
    }
