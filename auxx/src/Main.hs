module Main
       ( main
       ) where

import           Universum
import           Unsafe                (unsafeFromJust)

import           Formatting            (sformat, shown, (%))
import           Mockable              (Production, currentTime, runProduction)
import qualified Network.Transport.TCP as TCP (TCPAddr (..))
import           System.Wlog           (logInfo)

import qualified Pos.Client.CLI        as CLI
import           Pos.Core              (Timestamp (..))
import           Pos.Core.Context      (HasCoreConstants, giveStaticConsts)
import           Pos.Launcher          (NodeParams (..), applyConfigInfo,
                                        bracketNodeResources, runRealBasedMode)
import           Pos.Network.Types     (NetworkConfig (..), Topology (..),
                                        topologyDequeuePolicy, topologyEnqueuePolicy,
                                        topologyFailurePolicy)
import           Pos.Ssc.SscAlgo       (SscAlgo (GodTossingAlgo))
import           Pos.Util.UserSecret   (usVss)
import           Pos.WorkMode          (RealMode)

import           AuxxOptions           (AuxxOptions (..), getAuxxOptions)
import           Mode                  (AuxxContext (..), AuxxMode, AuxxSscType,
                                        CmdCtx (..), realModeToAuxx)
import           Plugin                (auxxPlugin)

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
        , ncTcpAddr = TCP.Unaddressable
        }

action :: HasCoreConstants => AuxxOptions -> Production ()
action opts@AuxxOptions {..} = do
    CLI.printFlags
    systemStart <- CLI.getNodeSystemStart $ CLI.sysStart commonArgs
    logInfo $ sformat ("System start time is "%shown) systemStart
    t <- currentTime
    logInfo $ sformat ("Current time is "%shown) (Timestamp t)
    nodeParams <-
        correctNodeParams opts <$> CLI.getNodeParams cArgs nArgs systemStart
    let vssSK = unsafeFromJust $ npUserSecret nodeParams ^. usVss
    let gtParams = CLI.gtSscParams cArgs vssSK (npBehaviorConfig nodeParams)
    bracketNodeResources @AuxxSscType nodeParams gtParams actionWithResources
  where
    cArgs@CLI.CommonNodeArgs {..} = aoCommonNodeArgs
    nArgs =
        CLI.NodeArgs {sscAlgo = GodTossingAlgo, behaviorConfigPath = Nothing}
    cmdCtx = CmdCtx {ccPeers = aoPeers}
    actionWithResources nr =
        runRealBasedMode toRealMode realModeToAuxx nr (auxxPlugin opts)
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
    applyConfigInfo (CLI.configInfo (aoCommonNodeArgs opts))
    giveStaticConsts $ runProduction $ action opts
