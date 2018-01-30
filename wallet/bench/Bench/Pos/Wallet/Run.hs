-- | Function for launching benchmark.

module Bench.Pos.Wallet.Run
    ( runBench
    ) where

import           Universum

import           Gauge.Main              (bench, runMode, nfIO)
import           Gauge.Main.Options      (Config (..), Mode (..), defaultConfig)

import           Bench.Pos.Wallet.Random (waitRandom)
import           Bench.Pos.Wallet.Types  (CompleteConfig (..), EndpointConfig (..),
                                          EndpointClient)

-- | Runs benchmark using particular client.
runBench
    :: EndpointClient
    -> CompleteConfig
    -> EndpointConfig
    -> IO ()
runBench endpointClient completeConfig EndpointConfig {..} =
    runMode DefaultMode
            config
            [benchName]
            [bench benchName $ nfIO ioForBench]
  where
    -- Since we treat a node as a blackbox, we measure complete IO-action.
    -- Currently this action includes:
    -- 1. Preparing TLS-connection.
    -- 2. Preparing, serialization and sending request.
    -- 3. Waiting for response.
    -- 4. Response deserealization.
    -- 5. Do something with response.
    -- TODO: Probably it should be changed.
    ioForBench = endpointClient completeConfig >> waitRandom (minDelayForCalls,
                                                              maxDelayForCalls)
    config = defaultConfig {
        timeLimit  = Just benchDuration,
        reportFile = Just pathToReportFile
    }
