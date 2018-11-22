module Main where

import           Universum

import           System.Remote.Monitoring (forkServer)

import           Pos.Client.CLI (NodeArgs (..), loggingParams, ekgParams)
import           Pos.Infra.Statistics.Ekg
import           Pos.Launcher (launchNode)
import           Pos.Util.CompileInfo (withCompileInfo)

import           Cardano.Wallet.Action (actionWithWallet)
import qualified Cardano.Wallet.LegacyAction as Legacy
import           Cardano.Wallet.Server.CLI (ChooseWalletBackend (..),
                     WalletStartupOptions (..), getWalletNodeOptions)

-- | The main entrypoint for the Wallet.
main :: IO ()
main = withCompileInfo $ do
    WalletStartupOptions cArgs wArgs <- getWalletNodeOptions
    let lArgs = loggingParams "node" cArgs
    let nArgs = NodeArgs { behaviorConfigPath = Nothing }

    putText "Wallet is starting..."

    case ekgParams cArgs of
      Just (EkgParams eHost ePort) -> do
        putText $ "EKG is starting on " <> show eHost <> ":" <> show ePort
        forkServer eHost ePort
        pure ()
      Nothing -> pure ()

    launchNode nArgs cArgs lArgs $ case wArgs of
        WalletLegacy p ->
            Legacy.actionWithWallet p

        WalletNew p ->
            actionWithWallet p
