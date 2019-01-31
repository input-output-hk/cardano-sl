{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module AutomatedTestRunner (Script, getGenesisConfig, loadNKeys, doUpdate, onStartup, on, runScript, ScriptRunnerOptions(..), endScript, srCommonNodeArgs, printbvd, ScriptParams(..)) where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async.Lifted.Safe (Async, async, wait)
import           Control.Exception (throw)
import           Control.Lens (to)
import           Control.Monad.STM (orElse)
import           Data.Constraint (Dict (Dict))
import           Data.Default (Default (def))
import qualified Data.HashMap.Strict as HM
import           Data.Ix (range)
import           Data.List ((!!))
import qualified Data.Map as Map
import qualified Data.Text as T
import           Data.Time.Units (fromMicroseconds)
import           Data.Version (showVersion)
import           Formatting (Format, int, sformat, stext, (%))
import           Options.Applicative (Parser, execParser, footerDoc, fullDesc,
                     header, help, helper, info, infoOption, long, progDesc,
                     switch)
import           Prelude (read)
import           System.Exit (ExitCode)
import           System.IO (BufferMode (LineBuffering), hPrint, hSetBuffering)
import qualified Turtle as T
import           Universum hiding (on)

import           Ntp.Client (NtpConfiguration)
import           Paths_cardano_sl (version)
import           Pos.Chain.Block (LastKnownHeaderTag)
import           Pos.Chain.Genesis as Genesis
                     (Config (configGeneratedSecrets, configProtocolMagic),
                     configEpochSlots)
import           Pos.Chain.Txp (TxpConfiguration)
import           Pos.Chain.Update (BlockVersion,
                     BlockVersionData (bvdMaxBlockSize, bvdMaxTxSize),
                     BlockVersionModifier, SoftwareVersion, SystemTag,
                     UpdateConfiguration, UpdateData, mkUpdateProposalWSign,
                     updateConfiguration)
import qualified Pos.Client.CLI as CLI
import           Pos.Client.KeyStorage (addSecretKey, getSecretKeysPlain)
import           Pos.Client.Update.Network (submitUpdateProposal)
import           Pos.Core (EpochIndex (EpochIndex), LocalSlotIndex, SlotCount,
                     SlotId (SlotId, siEpoch, siSlot), Timestamp (Timestamp),
                     difficultyL, getBlockCount, getChainDifficulty,
                     getEpochIndex, getEpochOrSlot, getSlotIndex,
                     mkLocalSlotIndex)
import           Pos.Crypto (emptyPassphrase, hash, hashHexF, noPassEncrypt,
                     withSafeSigners)
import           Pos.DB.BlockIndex (getTipHeader)
import           Pos.DB.Class (gsAdoptedBVData)
import           Pos.DB.DB (initNodeDBs)
import           Pos.DB.Txp (txpGlobalSettings)
import qualified Pos.GState as GS
import           Pos.Infra.Diffusion.Types (Diffusion, hoistDiffusion)
import           Pos.Infra.Network.Types (NetworkConfig (ncDequeuePolicy, ncEnqueuePolicy, ncFailurePolicy, ncTopology),
                     NodeId, Topology (TopologyAuxx), topologyDequeuePolicy,
                     topologyEnqueuePolicy, topologyFailurePolicy)
import           Pos.Infra.Shutdown (triggerShutdown, triggerShutdown')
import           Pos.Infra.Slotting.Util (defaultOnNewSlotParams, onNewSlot)
import           Pos.Launcher (HasConfigurations, InitModeContext, NodeParams (npBehaviorConfig, npNetworkConfig, npUserSecret),
                     NodeResources, WalletConfiguration, bracketNodeResources,
                     cfoSystemStart_L, loggerBracket, runNode, runRealMode,
                     withConfigurations)
import           Pos.Util (lensOf, logException)
import           Pos.Util.CompileInfo (CompileTimeInfo (ctiGitRevision),
                     HasCompileInfo, compileInfo, withCompileInfo)
import           Pos.Util.UserSecret (readUserSecret, usKeys, usPrimKey, usVss)
import           Pos.Util.Wlog (LoggerName)
import           Pos.WorkMode (EmptyMempoolExt, RealMode)
import           Serokell.Data.Memory.Units (Byte)

import           Brick.BChan (BChan, newBChan, readBChan, writeBChan)

import           BrickUI (runUI)
import           BrickUITypes (CustomEvent (CENodeInfo, CESlotStart, ProposalReply, QuitEvent),
                     NodeInfo (NodeInfo),
                     Reply (QueryProposals, TriggerShutdown),
                     SlotStart (SlotStart))
import           NodeControl (cleanupNodes, createNodes, genSystemStart, mkTopo)
import           PocMode (AuxxContext (AuxxContext, _acEventChan, _acNodeHandles, _acRealModeContext, _acScriptOptions, _acStatePath, _acTopology),
                     CompiledScript (slotTriggers, startupActions),
                     InputParams (InputParams, ipEventChan, ipReplyChan, ipScriptParams, ipStatePath),
                     InputParams2 (InputParams2, ip2EventChan, ip2ReplyChan, ip2ScriptParams, ip2StatePath),
                     PocMode, Script (runScriptMonad),
                     ScriptBuilder (ScriptBuilder, sbEpochSlots, sbGenesisConfig, sbScript),
                     ScriptParams (spRecentSystemStart, spScript, spStartCoreAndRelay, spTodo),
                     ScriptT (runScriptT), SlotTrigger (SlotTrigger),
                     realModeToAuxx, writeBrickChan)
import           Types (ScriptRunnerOptions (ScriptRunnerOptions),
                     ScriptRunnerUIMode (BrickUI, PrintUI), srCommonNodeArgs,
                     srPeers, srUiMode)

exampleToScript :: SlotCount -> Config -> Script () -> CompiledScript
exampleToScript epochSlots config example = sbScript $ snd $ runIdentity $ runStateT (runScriptT $ runScriptMonad example) (ScriptBuilder def epochSlots config)

scriptRunnerOptionsParser :: Parser ScriptRunnerOptions
scriptRunnerOptionsParser = do
  let
    disabledParser :: Parser Bool
    disabledParser = switch $ long "no-brickui" <> help "Disable brick based ui"
  disableBrick <- disabledParser
  commonNodeArgs <- CLI.commonNodeArgsParser
  peers <- many $ CLI.nodeIdOption "peer" "Address of a peer."
  pure $ ScriptRunnerOptions commonNodeArgs peers (if disableBrick then PrintUI else BrickUI)

getScriptRunnerOptions :: HasCompileInfo => IO ScriptRunnerOptions
getScriptRunnerOptions = execParser programInfo
  where
    programInfo = info (helper <*> versionOption <*> scriptRunnerOptionsParser) $
        fullDesc <> progDesc "Cardano SL CLI utilities."
                 <> header "CLI-based utilities (auxx)."
                 <> footerDoc (Just "todo")
    versionOption :: Parser (a -> a)
    versionOption = infoOption
      ("cardano-script-runner" <> showVersion version <> ", git revision " <> toString (ctiGitRevision compileInfo))
      (long "version" <> help "Show version.")

loggerName :: LoggerName
loggerName = "script-runner"

executeAction :: HasCompileInfo => ScriptRunnerOptions -> InputParams -> IO ()
executeAction opts inputParams = do
  let
    conf = CLI.configurationOptions (CLI.commonArgs cArgs)
    cArgs@CLI.CommonNodeArgs{CLI.cnaDumpGenesisDataPath,CLI.cnaDumpConfiguration} = opts ^. srCommonNodeArgs
  withConfigurations Nothing cnaDumpGenesisDataPath cnaDumpConfiguration conf (runWithConfig opts inputParams)

maybeAddPeers :: [NodeId] -> NodeParams -> NodeParams
maybeAddPeers [] params = params
maybeAddPeers peers nodeParams = addQueuePolicies $ nodeParams { npNetworkConfig = (npNetworkConfig nodeParams) { ncTopology = TopologyAuxx peers } }

-- used with maybeAddPeers to fix default policies after changing topology type
addQueuePolicies :: NodeParams -> NodeParams
addQueuePolicies nodeParams = do
  let
    topology = ncTopology $ npNetworkConfig nodeParams
  nodeParams { npNetworkConfig = (npNetworkConfig nodeParams)
    { ncEnqueuePolicy = topologyEnqueuePolicy topology
    , ncDequeuePolicy = topologyDequeuePolicy topology
    , ncFailurePolicy = topologyFailurePolicy topology
    }
  }

runWithConfig :: (HasCompileInfo, HasConfigurations) => ScriptRunnerOptions -> InputParams -> Genesis.Config -> WalletConfiguration -> TxpConfiguration -> NtpConfiguration -> IO ()
runWithConfig opts inputParams genesisConfig _walletConfig txpConfig _ntpConfig = do
  let
    nArgs = CLI.NodeArgs { CLI.behaviorConfigPath = Nothing}
  (nodeParams', _mSscParams) <- CLI.getNodeParams loggerName (opts ^. srCommonNodeArgs) nArgs (configGeneratedSecrets genesisConfig)
  let
    nodeParams = maybeAddPeers (opts ^. srPeers) $ nodeParams'
    vssSK = fromMaybe (error "no user secret given") (npUserSecret nodeParams ^. usVss)
    sscParams = CLI.gtSscParams (opts ^. srCommonNodeArgs) vssSK (npBehaviorConfig nodeParams)
    txpGS = txpGlobalSettings genesisConfig txpConfig
    initNDBs :: ReaderT InitModeContext IO ()
    initNDBs = initNodeDBs genesisConfig
  let
    inputParams' = InputParams2 (ipEventChan inputParams) (ipReplyChan inputParams) (ipScriptParams inputParams) (ipStatePath inputParams)
  bracketNodeResources genesisConfig nodeParams sscParams txpGS initNDBs (nodeResourceAction opts genesisConfig txpConfig inputParams')

nodeResourceAction :: (HasCompileInfo, HasConfigurations) => ScriptRunnerOptions -> Config -> TxpConfiguration -> InputParams2 -> NodeResources () -> IO ()
nodeResourceAction opts genesisConfig txpConfig inputParams nr = do
  handles <- newTVarIO mempty
  let
    -- cores run from 0-3, relays run from 0-0
    topo = mkTopo 3 0
  let
    toRealMode :: PocMode a -> RealMode EmptyMempoolExt a
    toRealMode auxxAction = do
      realModeContext <- ask
      lift $ runReaderT auxxAction $ AuxxContext
        { _acRealModeContext = realModeContext
        , _acEventChan = ip2EventChan inputParams
        , _acNodeHandles = handles
        , _acScriptOptions = opts
        , _acTopology = topo
        , _acStatePath = ip2StatePath inputParams
        }
    mkPlugins :: CompiledScript -> [ (Text, Diffusion PocMode -> PocMode ()) ]
    mkPlugins script = workers script genesisConfig inputParams
    mkPocMode :: Diffusion PocMode -> PocMode ()
    mkPocMode diffusion = do
      if spStartCoreAndRelay $ ip2ScriptParams inputParams
        then createNodes (spTodo $ ip2ScriptParams inputParams) opts
        else pure ()
      let epochSlots = configEpochSlots genesisConfig
      let finalscript = (exampleToScript epochSlots genesisConfig . spScript . ip2ScriptParams) inputParams
      runNode genesisConfig txpConfig nr (mkPlugins finalscript) diffusion
      cleanupNodes
    action :: Diffusion (RealMode ()) -> RealMode EmptyMempoolExt ()
    action diffusion = toRealMode (mkPocMode (hoistDiffusion realModeToAuxx toRealMode diffusion))
  runRealMode updateConfiguration genesisConfig txpConfig nr action

workers :: HasConfigurations => CompiledScript -> Genesis.Config -> InputParams2 -> [ (Text, Diffusion PocMode -> PocMode ()) ]
workers script genesisConfig InputParams2{ip2EventChan,ip2ReplyChan} =
  [ ( "worker1", worker1 genesisConfig script ip2EventChan)
  , ( "worker2", worker2 ip2EventChan)
  , ( "brick reply worker", brickReplyWorker ip2ReplyChan)
  ]

brickReplyWorker :: HasConfigurations => BChan Reply -> Diffusion PocMode -> PocMode ()
brickReplyWorker replyChan diffusion = do
  reply <- liftIO $ readBChan replyChan
  case reply of
    TriggerShutdown -> do
      triggerShutdown
    QueryProposals -> do
      uc <- view (lensOf @UpdateConfiguration)
      proposals <- GS.getConfirmedProposals uc Nothing
      allProp <- GS.getAllProposals
      writeBrickChan $ ProposalReply proposals allProp

  brickReplyWorker replyChan diffusion

worker2 :: HasConfigurations => BChan CustomEvent -> Diffusion PocMode -> PocMode ()
worker2 eventChan diffusion = do
  localTip  <- getTipHeader
  headerRef <- view (lensOf @LastKnownHeaderTag)
  mbHeader <- atomically $ readTVar headerRef `orElse` pure Nothing
  let
    globalHeight = view (difficultyL . to getChainDifficulty) <$> mbHeader
    localHeight = view (difficultyL . to getChainDifficulty) localTip
    f (Just v) = Just $ getBlockCount v
    f Nothing  = Nothing
  liftIO $ do
    writeBChan eventChan $ CENodeInfo $ NodeInfo (getBlockCount localHeight) (getEpochOrSlot localTip) (f globalHeight)
    threadDelay 10000
  worker2 eventChan diffusion

worker1 :: HasConfigurations => Genesis.Config -> CompiledScript -> BChan CustomEvent -> Diffusion (PocMode) -> PocMode ()
worker1 genesisConfig script eventChan diffusion = do
  let
    handler :: SlotId -> PocMode ()
    handler slotid = do
      liftIO $ writeBChan eventChan $ CESlotStart $ SlotStart (getEpochIndex $ siEpoch slotid) (getSlotIndex $ siSlot slotid)
      case Map.lookup slotid (slotTriggers script) of
        Just (SlotTrigger act) -> runAction act
        Nothing                -> pure ()
      pure ()
    errhandler :: Show e => e -> PocMode ()
    errhandler e = print e
    runAction :: (Dict HasConfigurations -> Diffusion PocMode -> PocMode ()) -> PocMode ()
    runAction act = do
      act Dict diffusion `catch` errhandler @SomeException
    realWorker = do
      mapM_ (\(SlotTrigger act) -> runAction act) (startupActions script)
      onNewSlot (configEpochSlots genesisConfig) defaultOnNewSlotParams handler
      pure ()
  realWorker `catch` errhandler @SomeException

runScript :: ScriptParams -> IO ()
runScript sp = T.with (T.mktempdir "/tmp" "script-runner") $ \stateDir -> withCompileInfo $ do
  systemStart <- genSystemStart 10
  let
    systemStartTs :: Timestamp
    systemStartTs = Timestamp $ fromMicroseconds $ (read systemStart) * 1000000
  opts' <- getScriptRunnerOptions
  let
    opts = if (spRecentSystemStart sp)
      then setSystemStartMutator systemStartTs opts'
      else opts'
  (eventChan, replyChan, asyncUi) <- runUI' opts
  let
    loggingParams = CLI.loggingParams loggerName (opts ^. srCommonNodeArgs)
  loggerBracket "script-runner" loggingParams . logException "script-runner" $ do
    let
      inputParams = InputParams eventChan replyChan sp (T.pack $ T.encodeString stateDir)
    executeAction opts inputParams
    pure ()
  liftIO $ writeBChan eventChan QuitEvent
  _finalState <- wait asyncUi
  --print finalState
  pure ()

runUI' :: ScriptRunnerOptions -> IO (BChan CustomEvent, BChan Reply, Async ())
runUI' opts = do
  case opts ^. srUiMode of
    BrickUI -> runUI
    PrintUI -> runDummyUI

runDummyUI :: IO (BChan CustomEvent, BChan Reply, Async ())
runDummyUI = do
  hSetBuffering stdout LineBuffering
  eventChan <- newBChan 10
  replyChan <- newBChan 10
  let
    go :: IO ()
    go = do
      reply <- liftIO $ readBChan eventChan
      case reply of
        QuitEvent -> pure ()
        _         -> go
  fakesync <- async go
  pure (eventChan, replyChan, fakesync)

getGenesisConfig :: Script Config
getGenesisConfig = sbGenesisConfig <$> get

data SlotCreationFailure = SlotCreationFailure { msg :: Text, slotsInEpoch :: SlotCount } deriving Show
instance Exception SlotCreationFailure where

onStartup :: (Dict HasConfigurations -> Diffusion PocMode -> PocMode ()) -> Script ()
onStartup action = do
  oldsb <- get
  let
    oldscript = sbScript oldsb
    script = oldscript {
      startupActions = [ SlotTrigger action ] <> (startupActions oldscript)
    }
    newsb = oldsb {
      sbScript = script
    }
  put newsb
  pure ()

endScript :: ExitCode -> PocMode ()
endScript code = do
  writeBrickChan QuitEvent
  triggerShutdown' code

on :: (Word64, Word16) -> (Dict HasConfigurations -> Diffusion PocMode -> PocMode ()) -> Script ()
on (epoch, slot) action = do
  oldsb <- get
  let
    todo = sbEpochSlots oldsb
    go :: Either Text LocalSlotIndex -> Script LocalSlotIndex
    go (Right localSlot) = pure localSlot
    go (Left err) = do
      throw $ SlotCreationFailure err todo
  localSlot <- go $ mkLocalSlotIndex todo slot
  let
    slot' = SlotId (EpochIndex epoch) localSlot
    oldscript = sbScript oldsb
    script = oldscript {
      slotTriggers = Map.insert slot' (SlotTrigger action) (slotTriggers oldscript)
    }
    newsb = oldsb {
      sbScript = script
    }
  put newsb

doUpdate :: HasConfigurations => Diffusion PocMode -> Config -> Int -> BlockVersion -> SoftwareVersion -> BlockVersionModifier -> PocMode ()
doUpdate diffusion genesisConfig keyIndex blockVersion softwareVersion blockVersionModifier = do
  let
    --tag = SystemTag "win64"
    updateData :: HM.HashMap SystemTag UpdateData
    updateData = HM.fromList [
        --(tag, UpdateData dummyHash dummyHash dummyHash dummyHash)
      ]
    voteAll = True
    errmsg :: Format r (Int -> Int -> r)
    errmsg = "Number of safe signers: " % int % ", expected " % int
    pm = configProtocolMagic genesisConfig
  skeys <- if voteAll then
      getSecretKeysPlain
    else do
      skey <- (!! keyIndex) <$> getSecretKeysPlain
      pure [ skey ]
  withSafeSigners skeys (pure emptyPassphrase) $ \ss -> do
    unless (length skeys == length ss) $ error $ sformat errmsg (length ss) (length skeys)
    let
      publisherSS = ss !! if not voteAll then 0 else keyIndex
      updateProposal = mkUpdateProposalWSign pm blockVersion blockVersionModifier softwareVersion updateData def publisherSS
      upid = hash updateProposal
    submitUpdateProposal pm diffusion ss updateProposal
    if not voteAll then
      putText (sformat ("Update proposal submitted, upId: "%hashHexF%"\n") upid)
    else
      putText (sformat ("Update proposal submitted along with votes, upId: "%hashHexF%"\n") upid)
    print updateProposal

loadNKeys :: Text -> Integer -> PocMode ()
loadNKeys stateDir n = do
  let
    fmt :: Format r (Text -> Integer -> r)
    fmt = stext % "/genesis-keys/generated-keys/rich/key" % int % ".sk"
    loadKey :: Integer -> PocMode ()
    loadKey x = do
      let
        keypath = sformat fmt stateDir x
      secret <- readUserSecret (T.unpack keypath)
      let
        sk = maybeToList $ secret ^. usPrimKey
        secret' = secret & usKeys %~ (++ map noPassEncrypt sk)
      let primSk = fromMaybe (error "Primary key not found") (secret' ^. usPrimKey)
      addSecretKey $ noPassEncrypt primSk
  mapM_ loadKey (range (0,n - 1))

printbvd :: Word64 -> Word16 -> Dict HasConfigurations -> Diffusion PocMode -> PocMode ()
printbvd epoch slot Dict _diffusion = do
  let
    bvdfmt :: Format r (Word64 -> Word16 -> Byte -> Byte -> r)
    bvdfmt = "epoch: "%int%" slot: "%int%" BVD: max-tx: " %int% ", max-block: " %int
  bar <- gsAdoptedBVData
  liftIO $ hPrint stderr $ sformat bvdfmt epoch slot (bvdMaxTxSize bar) (bvdMaxBlockSize bar)

setSystemStartMutator :: Timestamp -> ScriptRunnerOptions -> ScriptRunnerOptions
setSystemStartMutator systemStartTs optsin =
  -- sets the systemStart inside the ScriptRunnerOptions to the systemStart passed in
  optsin & srCommonNodeArgs . CLI.commonArgs_L . CLI.configurationOptions_L . cfoSystemStart_L .~ Just systemStartTs
