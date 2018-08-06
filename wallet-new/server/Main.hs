{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import           Universum

import           Cardano.Wallet.Launcher (startWalletNode)
import           Cardano.Wallet.Server.CLI (WalletStartupOptions (..),
                     getWalletNodeOptions)
import           Pos.Client.CLI.NodeOptions (NodeArgs (..))
import           Pos.Client.CLI.Params (loggingParams)
import           Pos.Util.CompileInfo (withCompileInfo)

-- | The main entrypoint for the Wallet.
main :: IO ()
main = withCompileInfo $ do
    putText "Wallet is starting...\n"
    wOpts <- getWalletNodeOptions
    let nArgs = NodeArgs { behaviorConfigPath = Nothing }
    let lArgs = loggingParams "node" (wsoNodeArgs wOpts)
    startWalletNode nArgs wOpts lArgs
