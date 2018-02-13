-- | Run 'GetWallets' client.

module Client.Pos.Wallet.Web.Endpoint.GetWallets
    ( getWalletsIO
    ) where

import           Universum

import           Client.Pos.Wallet.Web.Api         (getWallets)
import           Client.Pos.Wallet.Web.Run         (runEndpointClient)
import           Client.Pos.Wallet.Web.Analyze     (analyzeResponseIfNeeded, checkResponse)
import           Bench.Pos.Wallet.Types            (BenchEndpoint (..), CompleteConfig (..),
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
