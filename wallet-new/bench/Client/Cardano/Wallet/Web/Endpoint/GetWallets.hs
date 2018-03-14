-- | Run 'GetWallets' client.

module Client.Cardano.Wallet.Web.Endpoint.GetWallets
    ( getWalletsIO
    ) where

import           Universum

import           Client.Cardano.Wallet.Web.Api     (getWallets)
import           Client.Cardano.Wallet.Web.Run     (runEndpointClient)
import           Client.Cardano.Wallet.Web.Analyze (analyzeResponseIfNeeded, checkResponse)
import           Bench.Cardano.Wallet.Types        (BenchEndpoint (..), CompleteConfig (..),
                                                    Response, ResponseReport (..))

import           Pos.Wallet.Web.ClientTypes        (CWallet (..))

-- | Run 'GetWallets' client. As a result we will get a list of wallets.
getWalletsIO :: CompleteConfig -> IO ()
getWalletsIO conf@CompleteConfig {..} = do
    response <- runEndpointClient conf getWallets
    analyzeResponseIfNeeded GetWalletsBench conf $ analyze response

-- | Analyze response with list of wallets.
analyze
    :: Response [CWallet]
    -> ResponseReport
analyze response =
    checkResponse response
                  "Cannot get list of wallets"
                  $ \wallets -> ResponseReport $ show wallets
