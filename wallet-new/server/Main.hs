{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import           Universum

import           Data.Default (Default (..))
import           System.Wlog (LoggerName (..))

import           Cardano.Wallet.Launcher (startEdgeNode)
import           Cardano.Wallet.Server.CLI (getWalletNodeOptions)
import           Pos.Util.CompileInfo (withCompileInfo)

-- | The main entrypoint for the Wallet.
main :: IO ()
main = withCompileInfo $ do
    putText "Wallet is starting..."
    opts <- getWalletNodeOptions
    let lArgs = loggingParams loggerName (wsoNodeArgs opts)
    startEdgeNode def opts lArgs


