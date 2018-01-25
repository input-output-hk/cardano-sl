-- | Function for launching benchmark.

module Bench.Pos.Wallet.Run
    ( runBench
    ) where

import           Universum

import           Gauge.Main             (bench, runMode, nfIO)
import           Gauge.Main.Options     (Config (..), Mode (..), defaultConfig)
import           System.Random          (randomRIO)
import           Control.Concurrent     (threadDelay)

import           Bench.Pos.Wallet.Types (AdditionalBenchConfig (..),
                                         BenchEndpoint (..), EndpointClient)

-- | Runs benchmark using particular client.
runBench
    :: EndpointClient
    -> BenchEndpoint
    -> AdditionalBenchConfig
    -> IO ()
runBench endpointClient benchEp (AdditionalBenchConfig {..}) =
    runMode DefaultMode
            config
            [show benchEp]
            [bench benchName $
                nfIO (endpointClient >> wait (minDelayForCalls, maxDelayForCalls))]
  where
    config = defaultConfig {
        timeLimit  = Just benchDuration,
        reportFile = Just pathToReportFile
    }

-- | Waiting some random delay.
-- Values of @from@ and @to@ are already checked:
-- both are positive and @to@ is greater than @from@.
wait :: (Double, Double) -> IO ()
wait (from, to) =
    randomRIO (fromInMicrosec, toInMicrosec) >>= threadDelay
  where
    fromInMicrosec, toInMicrosec :: Int
    fromInMicrosec = truncate $ from * asMicrosec
    toInMicrosec   = truncate $ to * asMicrosec
    asMicrosec = 1000000
