-- | Run 'GetSyncProgress' client.

module Client.Cardano.Wallet.Web.Endpoint.GetSyncProgress
    ( getSyncProgressIO
    ) where

import           Universum

import           Client.Cardano.Wallet.Web.Api     (getSyncProgress)
import           Client.Cardano.Wallet.Web.Run     (runEndpointClient)
import           Client.Cardano.Wallet.Web.Analyze (analyzeResponseIfNeeded, checkResponse)
import           Bench.Cardano.Wallet.Types        (BenchEndpoint (..), CompleteConfig (..),
                                                    Response, ResponseReport (..))

import           Pos.Wallet.Web.ClientTypes        (SyncProgress (..))

-- | Run 'GetSyncProgress' client. As a result we will get a list of wallets.
getSyncProgressIO :: CompleteConfig -> IO ()
getSyncProgressIO conf@CompleteConfig {..} = do
    response <- runEndpointClient conf getSyncProgress
    analyzeResponseIfNeeded GetSyncProgressBench conf $ analyze response

-- | Analyze response with sync progress.
analyze
    :: Response SyncProgress
    -> ResponseReport
analyze response =
    checkResponse response
                  "Cannot get sync progress"
                  $ \syncProgress -> ResponseReport $ show syncProgress
