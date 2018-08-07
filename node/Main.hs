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

import           Ntp.Client (NtpConfiguration)

import           Pos.Binary ()
import           Pos.Chain.Ssc (SscParams)
import           Pos.Chain.Txp (TxpConfiguration)
import           Pos.Client.CLI (CommonNodeArgs (..), NodeArgs (..),
                     SimpleNodeArgs (..))
import qualified Pos.Client.CLI as CLI
import           Pos.Crypto (ProtocolMagic)
import           Pos.Launcher (HasConfigurations, NodeParams (..), runNodeReal,
                     withConfigurations)
import           Pos.Launcher.Configuration (AssetLockPath (..))
import           Pos.Launcher.Resource (getRealLoggerConfig)
import           Pos.Util (logException)
import           Pos.Util.CompileInfo (HasCompileInfo, withCompileInfo)
import qualified Pos.Util.Log as Log
import           Pos.Util.Trace (natTrace)
import           Pos.Util.Trace.Named (TraceNamed, appendName, logInfo,
                     namedTrace)
import           Pos.Util.UserSecret (usVss)
import           Pos.Worker.Update (updateTriggerWorker)

loggerName :: Log.LoggerName
loggerName = "node"

actionWithoutWallet
    :: ( HasConfigurations
       , HasCompileInfo
       , MonadIO m
       )
    => TraceNamed IO
    -> ProtocolMagic
    -> TxpConfiguration
    -> SscParams
    -> NodeParams
    -> m ()
actionWithoutWallet logTrace pm txpConfig sscParams nodeParams =
    liftIO $ runNodeReal logTrace pm txpConfig nodeParams sscParams
        [updateTriggerWorker (natTrace (lift . liftIO) logTrace)]

action
    :: ( HasConfigurations
       , HasCompileInfo
       , Log.WithLogger m
       , MonadCatch m
       )
    => TraceNamed IO
    -> SimpleNodeArgs
    -> ProtocolMagic
    -> TxpConfiguration
    -> NtpConfiguration
    -> m ()
action logTrace0 (SimpleNodeArgs (cArgs@CommonNodeArgs {..}) (nArgs@NodeArgs {..})) pm txpConfig ntpConfig = do
    CLI.printInfoOnStart logTrace cArgs ntpConfig txpConfig
    logInfo logTrace "Wallet is disabled, because software is built w/o it"
    currentParams <- CLI.getNodeParams loggerName cArgs nArgs

    let vssSK = fromJust $ npUserSecret currentParams ^. usVss
    let sscParams = CLI.gtSscParams cArgs vssSK (npBehaviorConfig currentParams)

    actionWithoutWallet logTrace0 pm txpConfig sscParams currentParams
      where
        logTrace = natTrace liftIO logTrace0

main :: IO ()
main = withCompileInfo $ do
    args@(CLI.SimpleNodeArgs commonNodeArgs _) <- CLI.getSimpleNodeOptions
    let loggingParams = CLI.loggingParams loggerName commonNodeArgs
    let conf = CLI.configurationOptions (CLI.commonArgs commonNodeArgs)
    let blPath = AssetLockPath <$> cnaAssetLockPath commonNodeArgs
    lh <- Log.setupLogging =<< getRealLoggerConfig loggingParams
    let logTrace = appendName loggerName $ namedTrace lh
    Log.loggerBracket lh loggerName . logException loggerName $
        withConfigurations (natTrace liftIO logTrace) blPath conf $
            action logTrace args
