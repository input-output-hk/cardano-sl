-- | Function for launching benchmark.

module Bench.Cardano.Wallet.Run
    ( runBench
    ) where

import           Universum

import           Gauge.Main                  (bench, runMode, nfIO)
import           Gauge.Main.Options          (Config (..), Mode (..), defaultConfig)

import           Bench.Cardano.Wallet.Random (waitRandom)
import           Bench.Cardano.Wallet.Types  (CompleteConfig (..), EndpointConfig (..),
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
    -- 5. Analyze response if needed.
    ioForBench = endpointClient completeConfig >> waitRandom (minDelayForCalls,
                                                              maxDelayForCalls)
    config = defaultConfig {
        minSamples = Just numberOfMeasures,
        csvFile    = Just pathToReportFile
    }
