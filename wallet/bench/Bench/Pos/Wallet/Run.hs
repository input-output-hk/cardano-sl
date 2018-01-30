-- | Function for launching benchmark.

module Bench.Pos.Wallet.Run
    ( runBench
    ) where

import           Universum

import           Gauge.Main              (bench, runMode, nfIO)
import           Gauge.Main.Options      (Config (..), Mode (..), defaultConfig)

import           Bench.Pos.Wallet.Random (waitRandom)
import           Bench.Pos.Wallet.Types  (CompleteConfig (..), EndpointConfig (..),
                                          BenchEndpoint (..), EndpointClient)

-- | Runs benchmark using particular client.
runBench
    :: EndpointClient
    -> BenchEndpoint
    -> CompleteConfig
    -> EndpointConfig
    -> IO ()
runBench endpointClient benchEp completeConfig EndpointConfig {..} =
    runMode DefaultMode
            config
            [show benchEp]
            [bench benchName $ nfIO ioForBench]
  where
    ioForBench = endpointClient completeConfig >> waitRandom (minDelayForCalls,
                                                              maxDelayForCalls)
    config = defaultConfig {
        timeLimit  = Just benchDuration,
        reportFile = Just pathToReportFile
    }
