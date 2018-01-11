module Bench.Pos.Wallet.Config
    ( AdditionalBenchConfig (..)
    , DelayRange
    , DelayFrom (..)
    , DelayTo (..)
    , EndpointClient
    ) where

import           Universum

import           Data.Csv                       (HasHeader (..), decode)



-- | Read benchmark configuration from the local .csv-file. The format is:
--
-- BenchName,BenchDuration,MinDelayBetweenCalls,MaxDelayBetweenCalls,PathToReportFile
-- GetHistoryBench,5.0,0.5,2.5,/tmp/GetHistoryBenchReport.csv
-- GetWalletsBench,5.5,0.2,2.1,/tmp/GetWalletsBenchReport.csv
getBenchConfig :: IO ()
getBenchConfig



{-
    let config = AdditionalBenchConfig {
          benchName         = "test"
        , benchDuration     = 5.0
        , delayBetweenCalls = (From 0.5, To 2.5)
        , pathToReportFile  = "/tmp/report.html"
        }
-}


extractConfigFor :: BenchEndpoint -> config -> AdditionalBenchConfig 
