module Main
       ( main
       ) where

import           Universum
import           Unsafe              (unsafeFromJust)

import           Formatting          (sformat, shown, (%))
import           Mockable            (Production, currentTime, runProduction)
import           System.Wlog         (logInfo)

import qualified Pos.Client.CLI      as CLI
import           Pos.Core            (Timestamp (..), gdStartTime, genesisData)
import           Pos.Launcher        (NodeParams (..), bracketNodeResources,
                                      loggerBracket, runRealBasedMode, withConfigurations)
import           Pos.Network.Types   (NetworkConfig (..), Topology (..),
                                      topologyDequeuePolicy, topologyEnqueuePolicy,
                                      topologyFailurePolicy)
import           Pos.Ssc.SscAlgo     (SscAlgo (GodTossingAlgo))
import           Pos.Util            (logException)
import           Pos.Util.UserSecret (usVss)
import           Pos.WorkMode        (RealMode)

import           AuxxOptions         (AuxxOptions (..), getAuxxOptions)
import           Mode                (AuxxContext (..), AuxxMode, AuxxSscType,
                                      CmdCtx (..), realModeToAuxx)
import           Plugin              (auxxPlugin)

-- 'NodeParams' obtained using 'CLI.getNodeParams' are not perfect for
-- Auxx, so we need to adopt them slightly.
correctNodeParams :: AuxxOptions -> NodeParams -> NodeParams
correctNodeParams AuxxOptions {..} np =
    np {npNetworkConfig = networkConfig}
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
        }

action :: AuxxOptions -> Production ()
action opts@AuxxOptions {..} = withConfigurations conf $ do
    CLI.printFlags
    logInfo $ sformat ("System start time is "%shown) $ gdStartTime genesisData
    t <- currentTime
    logInfo $ sformat ("Current time is "%shown) (Timestamp t)
    nodeParams <-
        correctNodeParams opts <$> CLI.getNodeParams cArgs nArgs
    let vssSK = unsafeFromJust $ npUserSecret nodeParams ^. usVss
    let gtParams = CLI.gtSscParams cArgs vssSK (npBehaviorConfig nodeParams)
    bracketNodeResources @AuxxSscType nodeParams gtParams $ \nr ->
        runRealBasedMode toRealMode realModeToAuxx nr (auxxPlugin opts)
  where
    cArgs@CLI.CommonNodeArgs {..} = aoCommonNodeArgs
    conf = CLI.configurationOptions (CLI.commonArgs cArgs)
    nArgs =
        CLI.NodeArgs {sscAlgo = GodTossingAlgo, behaviorConfigPath = Nothing}
    cmdCtx = CmdCtx {ccPeers = aoPeers}
    toRealMode :: AuxxMode a -> RealMode AuxxSscType a
    toRealMode auxxAction = do
        realModeContext <- ask
        let auxxContext =
                AuxxContext
                {acRealModeContext = realModeContext, acCmdCtx = cmdCtx}
        lift $ runReaderT auxxAction auxxContext

main :: IO ()
main = do
    opts <- getAuxxOptions
    let loggingParams = CLI.loggingParams "auxx" (aoCommonNodeArgs opts)
    loggerBracket loggingParams . logException "auxx" $ do
        runProduction $ action opts
