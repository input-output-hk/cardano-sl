{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import           Universum

import           System.Wlog (LoggerName (..))

import           Cardano.Wallet.Launcher (startWalletNode)
import           Cardano.Wallet.Server.CLI (getWalletNodeOptions)
import           Pos.Client.CLI.NodeOptions (NodeArgs (..))
import           Pos.Util.CompileInfo (withCompileInfo)


-- | The main entrypoint for the Wallet.
main :: IO ()
main = withCompileInfo $ do
    putText "Wallet is starting..."
    wOpts <- getWalletNodeOptions
    let nArgs = NodeArgs { behaviorConfigPath = Nothing }
    let lArgs = loggingParams loggerName (wsoNodeArgs wOpts)
    startWalletNode nArgs wOpts lArgs
