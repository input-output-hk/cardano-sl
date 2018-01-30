-- | Auxiliary types.

module Bench.Pos.Wallet.Types
    ( CompleteConfig (..)
    , EndpointConfig (..)
    , WalletAccount (..)
    , Wallet (..)
    , WalletsConfig (..)
    , CLOptions (..)
    , BenchEndpoint (..)
    , EndpointClient
    ) where

import           Universum

-- | Complete configuration for benchmarking.
data CompleteConfig = CompleteConfig
    { endpointsConfig :: !(NonEmpty EndpointConfig)
    , walletsConfig   :: !WalletsConfig
    , tlsPubCert      :: !ByteString
    , tlsPrivKey      :: !ByteString
    }

-- | Endpoint configuration, obtained from the .csv-file.
data EndpointConfig = EndpointConfig
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

-- | @WalletAccount@, for Wallets configuration, obtained from the .yaml-file.
data WalletAccount = WalletAccount
    { accountId :: !Text
    , addresses :: !(NonEmpty Text)
    }

-- | @Wallet@, for Wallets configuration, obtained from the .yaml-file.
data Wallet = Wallet
    { walletId :: !Text
    , accounts :: !(NonEmpty WalletAccount)
    }

-- | @WalletsConfig@, for Wallets configuration, obtained from the .yaml-file.
data WalletsConfig = WalletsConfig
    { wallets :: !(NonEmpty Wallet)
    }

-- | Command-line options for benchmarks.
data CLOptions = CLOptions
    { -- | Path to endpoints configuration file.
      pathToEndpointsConfig :: !FilePath
      -- | Path to wallets configuration file.
    , pathToWalletsConfig   :: !FilePath
      -- | Path to TLS public certificate.
    , pathToTLSPubCert      :: !FilePath
      -- | Path to TLS private key.
    , pathToTLSPrivKey      :: !FilePath
      -- | If True, run benchmarks concurrently.
    , runConcurrently       :: !Bool
    }

-- | Clarification which benchmark we want to use.
-- We need it for extracting particular configuration.
data BenchEndpoint
    = GetHistoryBench
    | GetWalletBench
    | GetWalletsBench
    | NewPaymentBench
    deriving (Show)

-- | Type synonym for client function: this function sends
-- requests to particular endpoint of the Wallet Web API.
type EndpointClient = CompleteConfig -> IO ()
