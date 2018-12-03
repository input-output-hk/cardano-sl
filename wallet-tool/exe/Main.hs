module Main where

import           Options.Applicative
import           System.Exit (ExitCode)
import           Universum

import           Cardano.Wallet.Client.CLI
import           Cardano.Wallet.Client.Easy

main :: IO ()
main = exitWith =<< uncurry run =<< execParser opts
  where
    opts = info (optionsParser <**> helper)
      ( fullDesc
        <> progDesc "Connect to the API of a Cardano SL wallet"
        <> header "connect-wallet - easy way to connect to a wallet" )

run :: ConnectConfig -> Action IO -> IO ExitCode
run cfg act = walletClientFromConfig cfg >>= runAction act
