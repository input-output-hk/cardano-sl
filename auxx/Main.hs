module Main
       ( main
       ) where

import           Universum
import           Unsafe (unsafeFromJust)

import           Control.Exception.Safe (handle)
import           Formatting (sformat, shown, (%))
import           JsonLog (jsonLog)
import           Mockable (Production, runProduction)
import qualified Network.Transport.TCP as TCP (TCPAddr (..))
import qualified System.IO.Temp as Temp
import           System.Wlog (LoggerName, logInfo)

import           Pos.Block.Configuration (recoveryHeadersMessage)
import qualified Pos.Client.CLI as CLI
import           Pos.Communication (OutSpecs)
import           Pos.Communication.Util (ActionSpec (..))
import           Pos.Configuration (networkConnectionTimeout)
import           Pos.Core (ConfigurationError, protocolConstants, protocolMagic)
import           Pos.DB.DB (initNodeDBs)
import           Pos.Diffusion.Full (FullDiffusionConfiguration (..), diffusionLayerFull)
import           Pos.Diffusion.Types (DiffusionLayer (..), hoistDiffusion)
import           Pos.Launcher (HasConfigurations, NodeParams (..), NodeResources,
                               bracketNodeResources, elimRealMode, loggerBracket, lpConsoleLog,
                               runNode, withConfigurations)
import           Pos.Logic.Full (logicLayerFull)
import           Pos.Logic.Types (LogicLayer (..), hoistLogic)
import           Pos.Network.Types (NetworkConfig (..), Topology (..), topologyDequeuePolicy,
                                    topologyEnqueuePolicy, topologyFailurePolicy)
import           Pos.Ntp.Configuration (NtpConfiguration)
import           Pos.Txp (txpGlobalSettings)
import           Pos.Update (lastKnownBlockVersion)
import           Pos.Util (logException)
import           Pos.Util.CompileInfo (HasCompileInfo, retrieveCompileTimeInfo, withCompileInfo)
import           Pos.Util.Config (ConfigurationException (..))
import           Pos.Util.UserSecret (usVss)
import           Pos.Util.Trace (wlogTrace)
import           Pos.Worker.Types (WorkerSpec)
import           Pos.WorkMode (EmptyMempoolExt, RealMode)

import           AuxxOptions (AuxxAction (..), AuxxOptions (..), AuxxStartMode (..), getAuxxOptions)
import           Mode (AuxxContext (..), AuxxMode)
import           Plugin (auxxPlugin, rawExec)
import           Repl (PrintAction, WithCommandAction (..), withAuxxRepl)

loggerName :: LoggerName
loggerName = "auxx"

-- 'NodeParams' obtained using 'CLI.getNodeParams' are not perfect for
-- Auxx, so we need to adapt them slightly.
correctNodeParams :: AuxxOptions -> NodeParams -> Production (NodeParams, Bool)
correctNodeParams AuxxOptions {..} np = do
    (dbPath, isTempDbUsed) <- case npDbPathM np of
        Nothing -> do
            tempDir <- liftIO $ Temp.getCanonicalTemporaryDirectory
            dbPath <- liftIO $ Temp.createTempDirectory tempDir "nodedb"
            logInfo $ sformat ("Temporary db created: "%shown) dbPath
            return (dbPath, True)
        Just dbPath -> do
            logInfo $ sformat ("Supplied db used: "%shown) dbPath
            return (dbPath, False)
    let np' = np
            { npNetworkConfig = networkConfig
            , npRebuildDb = npRebuildDb np || isTempDbUsed
            , npDbPathM = Just dbPath }
    return (np', isTempDbUsed)
  where
    topology = TopologyAuxx aoPeers
    networkConfig =
        NetworkConfig
        { ncDefaultPort = 3000
        , ncSelfName = Nothing
        , ncEnqueuePolicy = topologyEnqueuePolicy topology
        , ncDequeuePolicy = topologyDequeuePolicy topology
        , ncFailurePolicy = topologyFailurePolicy topology
        , ncTopology = topology
        , ncTcpAddr = TCP.Unaddressable
        }

runNodeWithSinglePlugin ::
       (HasConfigurations, HasCompileInfo)
    => NodeResources EmptyMempoolExt
    -> (WorkerSpec AuxxMode, OutSpecs)
    -> (WorkerSpec AuxxMode, OutSpecs)
runNodeWithSinglePlugin nr (plugin, plOuts) =
    runNode nr ([plugin], plOuts)

action :: HasCompileInfo => AuxxOptions -> Either WithCommandAction Text -> Production ()
action opts@AuxxOptions {..} command = do
    let pa = either printAction (const putText) command
    case aoStartMode of
        Automatic
            ->
                handle @_ @ConfigurationException (\_ -> runWithoutNode pa)
              . handle @_ @ConfigurationError (\_ -> runWithoutNode pa)
              $ withConfigurations conf (runWithConfig pa)
        Light
            -> runWithoutNode pa
        _   -> withConfigurations conf (runWithConfig pa)

  where
    runWithoutNode :: PrintAction Production -> Production ()
    runWithoutNode printAction = printAction "Mode: light" >> rawExec Nothing opts Nothing command

    runWithConfig :: HasConfigurations => PrintAction Production -> NtpConfiguration -> Production ()
    runWithConfig printAction ntpConfig = do
        printAction "Mode: with-config"
        CLI.printInfoOnStart aoCommonNodeArgs ntpConfig
        (nodeParams, tempDbUsed) <-
            correctNodeParams opts =<< CLI.getNodeParams loggerName cArgs nArgs

        let fdconf = FullDiffusionConfiguration
                { fdcProtocolMagic = protocolMagic
                , fdcProtocolConstants = protocolConstants
                , fdcRecoveryHeadersMessage = recoveryHeadersMessage
                , fdcLastKnownBlockVersion = lastKnownBlockVersion
                , fdcConvEstablishTimeout = networkConnectionTimeout
                , fdcTrace = wlogTrace "auxx"
                }

            toRealMode :: AuxxMode a -> RealMode EmptyMempoolExt a
            toRealMode auxxAction = do
                realModeContext <- ask
                let auxxContext =
                        AuxxContext
                        { acRealModeContext = realModeContext
                        , acTempDbUsed = tempDbUsed }
                lift $ runReaderT auxxAction auxxContext
        let vssSK = unsafeFromJust $ npUserSecret nodeParams ^. usVss
        let sscParams = CLI.gtSscParams cArgs vssSK (npBehaviorConfig nodeParams)
        bracketNodeResources nodeParams sscParams txpGlobalSettings initNodeDBs $ \nr -> do
            let runIO = runProduction . elimRealMode nr . toRealMode
            -- Monad here needs to be 'Production' (bracketNodeResources) so
            -- take it to real mode and then eliminate it.
            elimRealMode nr $ toRealMode $
                -- Here's an 'AuxxMode' thing, using a 'Logic AuxxMode' and
                -- doing a continuation in 'AuxxMode'
                logicLayerFull jsonLog $ \logicLayer ->
                    -- 'diffusionLayerFull' works in 'IO'. Luckily, we have
                    -- AuxxMode ~> IO and vice-versa (liftIO).
                    -- We hoist the 'Logic AuxxMode' using 'runIO' so that
                    -- the diffusion layer can use it.
                    liftIO $ diffusionLayerFull fdconf (npNetworkConfig nodeParams) Nothing (hoistLogic runIO (logic logicLayer)) $ \diffusionLayer -> runIO $ do
                        let modifier = if aoStartMode == WithNode then runNodeWithSinglePlugin nr else identity
                            (ActionSpec auxxModeAction, _) = modifier (auxxPlugin opts command)
                        -- We're back in 'AuxxMode' again. We run the logic
                        -- layer using a hoisted diffusion layer (liftIO).
                        runLogicLayer logicLayer (liftIO (runDiffusionLayer diffusionLayer (runIO (auxxModeAction (hoistDiffusion liftIO (diffusion diffusionLayer))))))
    cArgs@CLI.CommonNodeArgs {..} = aoCommonNodeArgs
    conf = CLI.configurationOptions (CLI.commonArgs cArgs)
    nArgs =
        CLI.NodeArgs {behaviorConfigPath = Nothing}

main :: IO ()
main = withCompileInfo $(retrieveCompileTimeInfo) $ do
    opts <- getAuxxOptions
    let disableConsoleLog
            | Repl <- aoAction opts =
                  -- Logging in the REPL disrupts the prompt,
                  -- so we disable it.
                  -- TODO: When LW-25 is resolved we also want
                  -- to be able to enable logging in REPL using
                  -- a Haskeline-compatible print action.
                  \lp -> lp { lpConsoleLog = Just False }
            | otherwise = identity
        loggingParams = disableConsoleLog $
            CLI.loggingParams loggerName (aoCommonNodeArgs opts)
    loggerBracket loggingParams . logException "auxx" $ do
        let runAction a = runProduction $ action opts a
        case aoAction opts of
            Repl    -> withAuxxRepl $ \c -> runAction (Left c)
            Cmd cmd -> runAction (Right cmd)
