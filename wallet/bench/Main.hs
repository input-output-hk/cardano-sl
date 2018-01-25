module Main
  ( main
  ) where

import           Universum

import           Control.Concurrent.Async       (async, wait)
import           Control.Monad                  (void)
import           System.IO                      (hSetEncoding, stdout, utf8)
import           Data.Maybe                     (maybe)

import           Bench.Pos.Wallet.Config        (extractConfigFor, getBenchConfig,
                                                 getOptions)
import           Bench.Pos.Wallet.Run           (runBench)
import           Bench.Pos.Wallet.Types         (BenchEndpoint (..), CLOptions (..))
import           Client.Pos.Wallet.Web.Endpoint (getHistoryIO, getWalletsIO, newPaymentIO)

-- | Example of benchmark command:
-- $ stack bench cardano-sl-wallet --benchmark-arguments "--ep-conf=$PWD/wallet/bench/config/Endpoints.csv"
--
-- It's a client, so we assume that the node (with Wallet Web API enabled)
-- is already running.
main :: IO ()
main = do
    CLOptions {..} <- getOptions
    conf <- getBenchConfig pathToEndpointsConf
    hSetEncoding stdout utf8
    if runConcurrently then do
        b1 <- async $ maybeRun getHistoryIO GetHistoryBench conf
        b2 <- async $ maybeRun getWalletsIO GetWalletsBench conf
        b3 <- async $ maybeRun newPaymentIO NewPaymentBench conf
        void $ wait b1
        void $ wait b2
        void $ wait b3
    else do
        maybeRun getHistoryIO GetHistoryBench conf
        maybeRun getWalletsIO GetWalletsBench conf
        maybeRun newPaymentIO NewPaymentBench conf
  where
    -- | Run benchmark if corresponding config is defined.
    maybeRun client endpoint configs =
        maybe (return ()) (runBench client endpoint) (extractConfigFor endpoint configs)
