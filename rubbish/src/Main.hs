module Main
       ( main
       ) where

import           Universum
import           Unsafe              (unsafeFromJust)

import           Formatting          (sformat, shown, (%))
import           Mockable            (Production, currentTime, runProduction)
import           System.Wlog         (logInfo)

import qualified Pos.Client.CLI      as CLI
import           Pos.Core            (Timestamp (..))
import           Pos.Core.Context    (HasCoreConstants, giveStaticConsts)
import           Pos.Launcher        (NodeParams (..), bracketNodeResources,
                                      runRealBasedMode)
import           Pos.Ssc.SscAlgo     (SscAlgo (GodTossingAlgo))
import           Pos.Util.UserSecret (usVss)
import           Pos.WorkMode        (RealMode)

import           Mode                (CmdCtx (..), RubbishContext (..), RubbishMode,
                                      RubbishSscType, realModeToRubbish)
import           Plugin              (rubbishPlugin)
import           RubbishOptions      (RubbishOptions (..), getRubbishOptions)

action :: HasCoreConstants => RubbishOptions -> Production ()
action opts@RubbishOptions {..} = do
    CLI.printFlags
    systemStart <- CLI.getNodeSystemStart $ CLI.sysStart commonArgs
    logInfo $ sformat ("System start time is " % shown) systemStart
    t <- currentTime
    logInfo $ sformat ("Current time is " % shown) (Timestamp t)
    nodeParams <- CLI.getNodeParams cArgs nArgs systemStart
    let vssSK = unsafeFromJust $ npUserSecret nodeParams ^. usVss
    let gtParams = CLI.gtSscParams cArgs vssSK (npBehaviorConfig nodeParams)
    bracketNodeResources @RubbishSscType nodeParams gtParams actionWithResources
  where
    cArgs@CLI.CommonNodeArgs {..} = roCommonNodeArgs
    nArgs =
        CLI.NodeArgs {sscAlgo = GodTossingAlgo, behaviorConfigPath = Nothing}
    cmdCtx = CmdCtx {ccPeers = roPeers}
    actionWithResources nr =
        runRealBasedMode toRealMode realModeToRubbish nr (rubbishPlugin opts)
    toRealMode :: RubbishMode a -> RealMode RubbishSscType a
    toRealMode rubbishAction = do
        realModeContext <- ask
        let rubbishContext =
                RubbishContext
                {rcRealModeContext = realModeContext, rcCmdCtx = cmdCtx}
        lift $ runReaderT rubbishAction rubbishContext

main :: IO ()
main = giveStaticConsts $ runProduction . action =<< getRubbishOptions
