module Bench.Pos.Wallet.Types
    ( AdditionalBenchConfig (..)
    , DelayRange
    , DelayFrom (..)
    , DelayTo (..)
    , BenchEndpoint (..)
    , EndpointClient
    ) where

import           Universum

-- | Additional benchmark configuration, obtained from the file.
data AdditionalBenchConfig = AdditionalBenchConfig
    { -- | Name of the benchmark, used in the report file.
      benchName         :: !String
      -- | Duration of benchmark, in seconds.
    , benchDuration     :: !Double
      -- | Range for the random delay generation, in seconds.
      -- This delay will be used as a pause between calls of a client.
    , delayBetweenCalls :: !DelayRange
      -- | Path to report HTML-file (if doesn't exist, it will be created).
    , pathToReportFile  :: !FilePath
    }

-- |
data BenchEndpoint
    = GetHistoryBench
    | GetWalletsBench
    | NewPaymentBench

-- | In the real-world delay between client calls is always random,
-- so we define the range for random value generation.
type DelayRange   = (DelayFrom, DelayTo)
newtype DelayFrom = From Double -- Minimum delay, in seconds.
newtype DelayTo   = To Double   -- Maximum delay, in seconds.

-- | Type synonym for client function: this function sends
-- requests to particular endpoint of the Wallet Web API.
type EndpointClient = IO ()
