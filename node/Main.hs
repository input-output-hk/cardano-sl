{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeOperators       #-}

module Main
       ( main
       ) where

import           Universum

import           Data.Maybe (fromJust)
import           Mockable (Production (..), runProduction)

import           Pos.Binary ()
import           Pos.Client.CLI (CommonNodeArgs (..), NodeArgs (..),
                     SimpleNodeArgs (..))
import qualified Pos.Client.CLI as CLI
import           Pos.Crypto (ProtocolMagic)
import           Pos.Infra.Ntp.Configuration (NtpConfiguration)
import           Pos.Launcher (HasConfigurations, NodeParams (..),
                     loggerBracket, runNodeReal, withConfigurations)
import           Pos.Launcher.Configuration (AssetLockPath (..))
import           Pos.Launcher.Resource (getRealLoggerConfig)
import           Pos.Ssc.Types (SscParams)
import           Pos.Util (logException)
import           Pos.Util.CompileInfo (HasCompileInfo, withCompileInfo)
import           Pos.Util.Trace (natTrace)
import           Pos.Util.Trace.Named (TraceNamed, appendName, logInfo,
                     namedTrace)
import           Pos.Util.UserSecret (usVss)
import           Pos.Worker.Update (updateTriggerWorker)

import qualified Pos.Util.Log as Log

loggerName :: Log.LoggerName
loggerName = "node"

actionWithoutWallet
    :: ( HasConfigurations
       , HasCompileInfo
       )
    => Log.LoggingHandler
    -> TraceNamed Production
    -> ProtocolMagic
    -> SscParams
    -> NodeParams
    -> Production ()
actionWithoutWallet lh logTrace pm sscParams nodeParams =
    runNodeReal lh pm nodeParams sscParams [updateTriggerWorker (natTrace lift logTrace)]

action
    :: ( HasConfigurations
       , HasCompileInfo
       )
    => Log.LoggingHandler
    -> TraceNamed IO
    -> SimpleNodeArgs
    -> NtpConfiguration
    -> ProtocolMagic
    -> Production ()
action lh logTrace (SimpleNodeArgs (cArgs@CommonNodeArgs {..}) (nArgs@NodeArgs {..})) ntpConfig pm = do
    CLI.printInfoOnStart cArgs ntpConfig
    let logTrace' = natTrace liftIO logTrace
    logInfo logTrace' "Wallet is disabled, because software is built w/o it"
    currentParams <- CLI.getNodeParams loggerName cArgs nArgs

    let vssSK = fromJust $ npUserSecret currentParams ^. usVss
    let sscParams = CLI.gtSscParams cArgs vssSK (npBehaviorConfig currentParams)

    actionWithoutWallet lh logTrace' pm sscParams currentParams

main :: IO ()
main = withCompileInfo $ do
    args@(CLI.SimpleNodeArgs commonNodeArgs _) <- CLI.getSimpleNodeOptions
    let loggingParams = CLI.loggingParams loggerName commonNodeArgs
    let conf = CLI.configurationOptions (CLI.commonArgs commonNodeArgs)
    let blPath = AssetLockPath <$> cnaAssetLockPath commonNodeArgs
    lh <- Log.setupLogging =<< getRealLoggerConfig loggingParams
    let logTrace = appendName loggerName $ namedTrace lh
    Log.usingLoggerName lh "node0" $ runProduction $
        loggerBracket lh "node1" $ (\a -> liftIO $ logException lh "nodeEx" a) .
                                   Log.usingLoggerName lh loggerName .
                                   runProduction $
                                       withConfigurations (natTrace liftIO logTrace) blPath conf $
                                           action lh logTrace args
