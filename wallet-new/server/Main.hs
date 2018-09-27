module Main where

import           Universum

import           Pos.Client.CLI (NodeArgs (..), loggingParams)
import           Pos.Launcher (launchNode)
import           Pos.Util.CompileInfo (withCompileInfo)

import           Cardano.Wallet.Action (actionWithWallet)
import           Cardano.Wallet.Server.CLI (ChooseWalletBackend (..),
                     WalletStartupOptions (..), getWalletNodeOptions)


-- | The main entrypoint for the Wallet.
main :: IO ()
main = withCompileInfo $ do
    WalletStartupOptions cArgs wArgs <- getWalletNodeOptions
    let lArgs = loggingParams "node" cArgs
    let nArgs = NodeArgs { behaviorConfigPath = Nothing }

    putText "Wallet is starting..."

    launchNode nArgs cArgs lArgs $ case wArgs of
        WalletLegacy _ ->
            error "Legacy wallet is now disabled. This option will be removed soon."

        WalletNew p ->
            actionWithWallet p
