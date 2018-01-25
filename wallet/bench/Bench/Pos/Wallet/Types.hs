-- | Auxiliary types.

module Bench.Pos.Wallet.Types
    ( AdditionalBenchConfig (..)
    , CLOptions (..)
    , BenchEndpoint (..)
    , EndpointClient
    ) where

import           Universum

-- | Additional benchmark configuration, obtained from the file.
data AdditionalBenchConfig = AdditionalBenchConfig
    { -- | Name of the benchmark, used in the report file.
      benchName        :: !String
      -- | Duration of benchmark, in seconds.
    , benchDuration    :: !Double
      -- | Minimal value for the random delay generation, in seconds.
      -- This delay will be used as a pause between calls of a client.
    , minDelayForCalls :: !Double
      -- | Maximal value for the random delay generation, in seconds.
      -- This delay will be used as a pause between calls of a client.
    , maxDelayForCalls :: !Double
      -- | Path to report file (if doesn't exist, it will be created).
    , pathToReportFile :: !FilePath
    }

-- | Command-line options for benchmarks.
data CLOptions = CLOptions
    { -- | Path to benchmark configuration file.
      pathToEndpointsConf :: !FilePath
      -- | If True, run benchmarks concurrently.
    , runConcurrently     :: !Bool
    }

-- | Clarification which benchmark we want to use.
-- We need it for extracting particular configuration.
data BenchEndpoint
    = GetHistoryBench
    | GetWalletsBench
    | NewPaymentBench
    deriving (Show)

-- | Type synonym for client function: this function sends
-- requests to particular endpoint of the Wallet Web API.
type EndpointClient = IO ()
