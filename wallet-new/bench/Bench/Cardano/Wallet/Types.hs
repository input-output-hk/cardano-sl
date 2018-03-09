-- | Auxiliary types.

module Bench.Cardano.Wallet.Types
    ( CompleteConfig (..)
    , EndpointConfig (..)
    , WalletAccount (..)
    , Wallet (..)
    , WalletsConfig (..)
    , CLOptions (..)
    , BenchEndpoint (..)
    , EndpointClient
    , Response
    , ResponseReport (..)
    ) where

import           Universum

import           Pos.Wallet.Web.ClientTypes (Addr, CId (..), CAccountId (..), Wal)
import           Pos.Wallet.Web.Error.Types (WalletError (..))

-- | Complete configuration for benchmarking.
data CompleteConfig = CompleteConfig
    { endpointsConfig      :: !(NonEmpty EndpointConfig)
    , walletsConfig        :: !WalletsConfig
    , tlsPubCert           :: !ByteString
    , tlsPrivKey           :: !ByteString
    , needResponseAnalysis :: !Bool
    }

-- | Endpoint configuration, obtained from the .csv-file.
data EndpointConfig = EndpointConfig
    { -- | Name of the benchmark, used in the report file.
      benchName             :: !String
      -- | Number of measures in benchmark.
    , numberOfMeasures      :: !Int
      -- | Minimal value for the random delay generation, in seconds.
      -- This delay will be used as a pause between calls of a client.
    , minDelayForCalls      :: !Double
      -- | Maximal value for the random delay generation, in seconds.
      -- This delay will be used as a pause between calls of a client.
    , maxDelayForCalls      :: !Double
      -- | Path to report file (if doesn't exist, it will be created).
    , pathToReportFile      :: !FilePath
      -- | Path to response reports file (if doesn't exist, it will be created).
    , pathToResponseReports :: !FilePath
    }

-- | @WalletAccount@, for Wallets configuration, obtained from the .yaml-file.
data WalletAccount = WalletAccount
    { accountId :: !CAccountId
    , addresses :: !(NonEmpty (CId Addr))
    }

-- | @Wallet@, for Wallets configuration, obtained from the .yaml-file.
data Wallet = Wallet
    { walletId :: !(CId Wal)
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
      -- | If True, we must analyze responses from server.
    , analyzeResponse       :: !Bool
    }

-- | Clarification which benchmark we want to use.
-- We need it for extracting particular configuration.
data BenchEndpoint
    = GetAccountsBench
    | GetHistoryBench
    | GetSyncProgressBench
    | GetWalletBench
    | GetWalletsBench
    | IsValidAddressBench
    | NewAddressBench
    | NewPaymentBench
    | NewWalletBench
    deriving (Show)

-- | Type synonym for client function: this function sends
-- requests to particular endpoint of the Wallet Web API.
type EndpointClient = CompleteConfig -> IO ()

-- | Response from the server. All endpoints return this type.
type Response dataWeNeed = Either Text (Either WalletError dataWeNeed)

-- | Report about correctness of response from the server.
newtype ResponseReport = ResponseReport Text
    deriving Show
