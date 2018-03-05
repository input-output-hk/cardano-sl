module Main
  ( main
  ) where

import           Universum

import           Control.Concurrent.Async           (async, wait)
import           System.IO                          (hSetEncoding, stdout, utf8)
import qualified Data.ByteString                    as BS

import           Bench.Cardano.Wallet.Config        (getOptions, extractEndpointConfigFor,
                                                     getEndpointsConfig, getWalletsConfig)
import           Bench.Cardano.Wallet.Run           (runBench)
import           Bench.Cardano.Wallet.Types         (BenchEndpoint (..), CompleteConfig (..),
                                                     CLOptions (..))
import           Client.Cardano.Wallet.Web.Endpoint (getAccountsIO, getHistoryIO, getSyncProgressIO,
                                                     getWalletIO, getWalletsIO,
                                                     isValidAddressIO, newAddressIO,
                                                     newPaymentIO, newWalletIO)

-- | Example of benchmark command:
-- $ stack bench cardano-sl-wallet --benchmark-arguments  \
--      "--tls-pub-cert=$PWD/scripts/tls-files/ca.crt     \
--       --tls-priv-key=$PWD/scripts/tls-files/server.key \
--       --wal-conf=$PWD/wallet/bench/config/Wallets.yaml \
--       --ep-conf=$PWD/wallet/bench/config/Endpoints.csv"
-- To see arguments' description, run:
-- $ stack bench cardano-sl-wallet --benchmark-arguments "--help"
--
-- It's a client, so we assume that the node (with Wallet Web API enabled)
-- is already running. During benchmarking we treat a node as a blackbox.
main :: IO ()
main = do
    hSetEncoding stdout utf8
    CLOptions {..} <- getOptions
    conf <- CompleteConfig <$> getEndpointsConfig pathToEndpointsConfig
                           <*> getWalletsConfig pathToWalletsConfig
                           <*> BS.readFile pathToTLSPubCert
                           <*> BS.readFile pathToTLSPrivKey
                           <*> return analyzeResponse
    let benchmarks = [ maybeRun getAccountsIO     GetAccountsBench     conf
                     , maybeRun getHistoryIO      GetHistoryBench      conf
                     , maybeRun getSyncProgressIO GetSyncProgressBench conf
                     , maybeRun getWalletIO       GetWalletBench       conf
                     , maybeRun getWalletsIO      GetWalletsBench      conf
                     , maybeRun isValidAddressIO  IsValidAddressBench  conf
                     , maybeRun newAddressIO      NewAddressBench      conf
                     , maybeRun newPaymentIO      NewPaymentBench      conf
                     , maybeRun newWalletIO       NewWalletBench       conf
                     ]
    if runConcurrently then do
        asyncs <- forM benchmarks async
        forM_ asyncs wait
    else
        sequence_ benchmarks
  where
      -- | Run benchmark if config for corresponding endpoint is defined.
    maybeRun client endpoint conf =
        whenJust (extractEndpointConfigFor endpoint conf) $ runBench client conf
