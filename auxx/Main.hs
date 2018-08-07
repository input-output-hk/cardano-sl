module Main
       ( main
       ) where

import           Universum

import           Control.Exception.Safe (handle)
import           Data.Maybe (fromMaybe)
import           Formatting (sformat, shown, (%))
import qualified Network.Transport.TCP as TCP (TCPAddr (..))
import qualified System.IO.Temp as Temp

import           Ntp.Client (NtpConfiguration)

import           Pos.Chain.Txp (TxpConfiguration)
import qualified Pos.Client.CLI as CLI
import           Pos.Context (NodeContext (..))
import           Pos.Core (ConfigurationError, epochSlots)
import           Pos.Crypto (ProtocolMagic)
import           Pos.DB.DB (initNodeDBs)
import           Pos.DB.Txp (txpGlobalSettings)
import           Pos.Infra.Diffusion.Types (Diffusion, hoistDiffusion)
import           Pos.Infra.Network.Types (NetworkConfig (..), Topology (..),
                     topologyDequeuePolicy, topologyEnqueuePolicy,
                     topologyFailurePolicy)
import           Pos.Launcher (HasConfigurations, NodeParams (..),
                     NodeResources (..), bracketNodeResources, lpConsoleLog,
                     runNode, runRealMode, withConfigurations)
import           Pos.Launcher.Resource (getRealLoggerConfig)
import           Pos.Util (logException)
import           Pos.Util.CompileInfo (HasCompileInfo, withCompileInfo)
import           Pos.Util.Config (ConfigurationException (..))
import           Pos.Util.Log (LoggerName, loggerBracket, setupLogging)
import           Pos.Util.Trace (natTrace)
import           Pos.Util.Trace.Named (TraceNamed, appendName, logInfo,
                     namedTrace)
import           Pos.Util.UserSecret (usVss)
import           Pos.WorkMode (EmptyMempoolExt, RealMode)

import           AuxxOptions (AuxxAction (..), AuxxOptions (..),
                     AuxxStartMode (..), getAuxxOptions)
import           Mode (AuxxContext (..), AuxxMode, realModeToAuxx)
import           Plugin (auxxPlugin, rawExec)
import           Repl (PrintAction, WithCommandAction (..), withAuxxRepl)

loggerName :: LoggerName
loggerName = "auxx"

-- 'NodeParams' obtained using 'CLI.getNodeParams' are not perfect for
-- Auxx, so we need to adapt them slightly.
correctNodeParams :: TraceNamed IO -> AuxxOptions -> NodeParams -> IO (NodeParams, Bool)
correctNodeParams logTrace AuxxOptions {..} np = do
    (dbPath, isTempDbUsed) <- case npDbPathM np of
        Nothing -> do
            tempDir <- Temp.getCanonicalTemporaryDirectory
            dbPath <- Temp.createTempDirectory tempDir "nodedb"
            logInfo logTrace $ sformat ("Temporary db created: "%shown) dbPath
            return (dbPath, True)
        Just dbPath -> do
            logInfo logTrace $ sformat ("Supplied db used: "%shown) dbPath
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
    => TraceNamed IO
    -> ProtocolMagic
    -> TxpConfiguration
    -> NodeResources EmptyMempoolExt
    -> (Diffusion AuxxMode -> AuxxMode ())
    -> Diffusion AuxxMode -> AuxxMode ()
runNodeWithSinglePlugin logTrace pm txpConfig nr plugin =
    runNode (natTrace liftIO logTrace) pm txpConfig nr [plugin]

action
    :: HasCompileInfo
    => TraceNamed IO
    -> AuxxOptions
    -> Either WithCommandAction Text
    -> IO ()
action logTrace opts@AuxxOptions {..} command = do
    let pa = either printAction (const putText) command
    case aoStartMode of
        Automatic
            ->
                handle @_ @ConfigurationException (\_ -> runWithoutNode pa)
              . handle @_ @ConfigurationError (\_ -> runWithoutNode pa)
              $ withConfigurations logTrace Nothing conf (runWithConfig pa)
        Light
            -> runWithoutNode pa
        _   -> withConfigurations logTrace Nothing conf (runWithConfig pa)

  where
    runWithoutNode :: PrintAction IO -> IO ()
    runWithoutNode printAction = printAction "Mode: light"
        >> rawExec logTrace Nothing Nothing Nothing opts Nothing command

    runWithConfig
        :: HasConfigurations
        => PrintAction IO
        -> ProtocolMagic
        -> TxpConfiguration -> NtpConfiguration
        -> IO ()
    runWithConfig printAction pm txpConfig ntpConfig = do
        printAction "Mode: with-config"
        CLI.printInfoOnStart logTrace aoCommonNodeArgs ntpConfig txpConfig
        (nodeParams, tempDbUsed) <-
            correctNodeParams logTrace opts =<< CLI.getNodeParams loggerName cArgs nArgs

        let toRealMode :: AuxxMode a -> RealMode EmptyMempoolExt a
            toRealMode auxxAction = do
                realModeContext <- ask
                let auxxContext =
                        AuxxContext
                        { acRealModeContext = realModeContext
                        , acTempDbUsed = tempDbUsed }
                lift $ runReaderT auxxAction auxxContext
            vssSK = fromMaybe (error "no user secret given")
                              (npUserSecret nodeParams ^. usVss)
            sscParams = CLI.gtSscParams cArgs vssSK (npBehaviorConfig nodeParams)

        bracketNodeResources logTrace nodeParams sscParams
                             (txpGlobalSettings pm txpConfig)
                             (initNodeDBs pm epochSlots) $ \nr ->
            let NodeContext {..} = nrContext nr
                modifier = if aoStartMode == WithNode
                           then runNodeWithSinglePlugin logTrace pm txpConfig nr
                           else identity
                auxxModeAction = modifier (auxxPlugin (natTrace liftIO logTrace) pm txpConfig opts command)
             in runRealMode logTrace pm txpConfig nr $ \diffusion ->
                    toRealMode (auxxModeAction (hoistDiffusion realModeToAuxx toRealMode diffusion))

    cArgs@CLI.CommonNodeArgs {..} = aoCommonNodeArgs
    conf = CLI.configurationOptions (CLI.commonArgs cArgs)
    nArgs =
        CLI.NodeArgs {behaviorConfigPath = Nothing}

main :: IO ()
main = withCompileInfo $ do
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
    lh <- setupLogging =<< getRealLoggerConfig loggingParams
    let logTrace = appendName loggerName $ namedTrace lh
    loggerBracket lh loggerName . logException loggerName $ do
        let runAction a = action (natTrace liftIO logTrace) opts a
        case aoAction opts of
            Repl    -> liftIO $ withAuxxRepl $ \c -> runAction (Left c)
            Cmd cmd -> liftIO $ runAction (Right cmd)
