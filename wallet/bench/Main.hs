module Main
  ( main
  ) where

import           Universum

import           Control.Concurrent.Async       (async, wait)
import           System.IO                      (hSetEncoding, stdout, utf8)

import           Bench.Pos.Wallet.Config        (extractConfigFor, getBenchConfig)
import           Bench.Pos.Wallet.Run           (runBench)
import           Bench.Pos.Wallet.Types         (AdditionalBenchConfig (..),
                                                 DelayFrom (..), DelayTo (..),
                                                 BenchEndpoint (..))
import           Client.Pos.Wallet.Web.Endpoint (getHistoryIO, getWalletsIO, newPaymentIO)

-- | Use `stack bench cardano-sl-wallet` to run this benchmark.
-- Since it's a client, we assume that the node (with Wallet Web API enabled)
-- is already running.
main :: IO ()
main = do
    config <- getBenchConfig
    hSetEncoding stdout utf8
    putText "Wallet benchmarking begin..." 
    b1 <- async $ runBench getHistoryIO (extractConfigFor GetHistoryBench config)
    b2 <- async $ runBench getWalletsIO (extractConfigFor GetWalletsBench config)
    b3 <- async $ runBench newPaymentIO (extractConfigFor NewPaymentBench config)
    _ <- wait b1
    _ <- wait b2
    _ <- wait b3
    putText $ "Wallet benchmarking done, please see reports in directory " <> "BLA"
