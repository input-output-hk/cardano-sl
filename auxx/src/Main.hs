module Main
       ( main
       ) where

import           Universum
import           Unsafe                (unsafeFromJust)

import           Formatting            (sformat, shown, (%))
import           Mockable              (Production, currentTime, runProduction)
import qualified Network.Transport.TCP as TCP (TCPAddr (..))
import qualified System.IO.Temp        as Temp
import           System.Wlog           (logInfo)

import qualified Pos.Client.CLI        as CLI
import           Pos.Communication     (OutSpecs, WorkerSpec)
import           Pos.Core              (Timestamp (..), gdStartTime, genesisData)
import           Pos.Launcher          (HasConfigurations, NodeParams (..), NodeResources,
                                        bracketNodeResources, runNode, runRealBasedMode,
                                        withConfigurations)
import           Pos.Network.Types     (NetworkConfig (..), Topology (..),
                                        topologyDequeuePolicy, topologyEnqueuePolicy,
                                        topologyFailurePolicy)
import           Pos.Ssc.SscAlgo       (SscAlgo (GodTossingAlgo))
import           Pos.Util.CompileInfo  (HasCompileInfo, retrieveCompileTimeInfo,
                                        withCompileInfo)
import           Pos.Util.UserSecret   (usVss)
import           Pos.WorkMode          (EmptyMempoolExt, RealMode)

import           AuxxOptions           (AuxxOptions (..), getAuxxOptions)
import           Mode                  (AuxxContext (..), AuxxMode, AuxxSscType,
                                        CmdCtx (..), realModeToAuxx)
import           Plugin                (auxxPlugin)

-- 'NodeParams' obtained using 'CLI.getNodeParams' are not perfect for
-- Auxx, so we need to adapt them slightly.
correctNodeParams :: AuxxOptions -> NodeParams -> Production (NodeParams, Bool)
correctNodeParams AuxxOptions {..} np = do
    (dbPath, isTempDbUsed) <- case npDbPathM np of
        Nothing -> do
            tempDir <- liftIO $ Temp.getCanonicalTemporaryDirectory
            dbPath <- liftIO $ Temp.createTempDirectory tempDir "nodedb"
            putStrLn $ -- TODO: use logInfo after CSL-1693
                sformat ("Temporary db created: "%shown) dbPath
            return (dbPath, True)
        Just dbPath -> do
            putStrLn $ -- TODO: use logInfo after CSL-1693
                sformat ("Supplied db used: "%shown) dbPath
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
    => NodeResources AuxxSscType EmptyMempoolExt AuxxMode
    -> (WorkerSpec AuxxMode, OutSpecs)
    -> (WorkerSpec AuxxMode, OutSpecs)
runNodeWithSinglePlugin nr (plugin, plOuts) =
    runNode nr ([plugin], plOuts)

action :: HasCompileInfo => AuxxOptions -> Production ()
action opts@AuxxOptions {..} = withConfigurations conf $ do
    CLI.printFlags
    logInfo $ sformat ("System start time is "%shown) $ gdStartTime genesisData
    t <- currentTime
    logInfo $ sformat ("Current time is "%shown) (Timestamp t)
    (nodeParams, tempDbUsed) <-
        correctNodeParams opts =<< CLI.getNodeParams cArgs nArgs
    let
        toRealMode :: AuxxMode a -> RealMode AuxxSscType EmptyMempoolExt a
        toRealMode auxxAction = do
            realModeContext <- ask
            let auxxContext =
                    AuxxContext
                    { acRealModeContext = realModeContext
                    , acCmdCtx = cmdCtx
                    , acTempDbUsed = tempDbUsed }
            lift $ runReaderT auxxAction auxxContext
    let vssSK = unsafeFromJust $ npUserSecret nodeParams ^. usVss
    let gtParams = CLI.gtSscParams cArgs vssSK (npBehaviorConfig nodeParams)
    bracketNodeResources @AuxxSscType nodeParams gtParams $ \nr ->
        runRealBasedMode toRealMode realModeToAuxx nr $
            (if aoNodeEnabled then runNodeWithSinglePlugin nr else identity)
            (auxxPlugin opts)
  where
    cArgs@CLI.CommonNodeArgs {..} = aoCommonNodeArgs
    conf = CLI.configurationOptions (CLI.commonArgs cArgs)
    nArgs =
        CLI.NodeArgs {sscAlgo = GodTossingAlgo, behaviorConfigPath = Nothing}
    cmdCtx = CmdCtx {ccPeers = aoPeers}

main :: IO ()
main =
    withCompileInfo $(retrieveCompileTimeInfo) $
    getAuxxOptions >>= runProduction . action
