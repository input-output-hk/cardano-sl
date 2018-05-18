module Main
       ( main
       ) where

import           Universum
import           Unsafe (unsafeFromJust)

import           Control.Exception.Safe (handle)
import           Formatting (sformat, shown, (%))
import           Mockable (Production (..), runProduction)
import qualified Network.Transport.TCP as TCP (TCPAddr (..))
import qualified System.IO.Temp as Temp
import           System.Wlog (LoggerName, logInfo)

import qualified Pos.Client.CLI as CLI
import           Pos.Context (NodeContext (..))
import           Pos.Core (ConfigurationError)
import           Pos.DB.DB (initNodeDBs)
import           Pos.Diffusion.Types (Diffusion, hoistDiffusion)
import           Pos.Launcher (HasConfigurations, NodeParams (..), NodeResources (..),
                               bracketNodeResources, runRealMode, loggerBracket, lpConsoleLog,
                               runNode, withConfigurations)
import           Pos.Network.Types (NetworkConfig (..), Topology (..), topologyDequeuePolicy,
                                    topologyEnqueuePolicy, topologyFailurePolicy)
import           Pos.Ntp.Configuration (NtpConfiguration)
import           Pos.Txp (txpGlobalSettings)
import           Pos.Util (logException)
import           Pos.Util.CompileInfo (HasCompileInfo, retrieveCompileTimeInfo, withCompileInfo)
import           Pos.Util.Config (ConfigurationException (..))
import           Pos.Util.UserSecret (usVss)
import           Pos.WorkMode (EmptyMempoolExt, RealMode)

import           AuxxOptions (AuxxAction (..), AuxxOptions (..), AuxxStartMode (..), getAuxxOptions)
import           Mode (AuxxContext (..), AuxxMode, realModeToAuxx)
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
    -> (Diffusion AuxxMode -> AuxxMode ())
    -> Diffusion AuxxMode -> AuxxMode ()
runNodeWithSinglePlugin nr plugin =
    runNode nr [plugin]

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

        let toRealMode :: AuxxMode a -> RealMode EmptyMempoolExt a
            toRealMode auxxAction = do
                realModeContext <- ask
                let auxxContext =
                        AuxxContext
                        { acRealModeContext = realModeContext
                        , acTempDbUsed = tempDbUsed }
                lift $ runReaderT auxxAction auxxContext

            vssSK = unsafeFromJust $ npUserSecret nodeParams ^. usVss
            sscParams = CLI.gtSscParams cArgs vssSK (npBehaviorConfig nodeParams)

        bracketNodeResources nodeParams sscParams txpGlobalSettings initNodeDBs $ \nr -> Production $
            let NodeContext {..} = nrContext nr
                modifier = if aoStartMode == WithNode
                           then runNodeWithSinglePlugin nr
                           else identity
                auxxModeAction = modifier (auxxPlugin opts command)
             in runRealMode nr $ \diffusion ->
                    toRealMode (auxxModeAction (hoistDiffusion realModeToAuxx diffusion))

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
