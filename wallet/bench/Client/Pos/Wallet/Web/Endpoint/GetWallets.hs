-- | Run 'GetWallets' client.

module Client.Pos.Wallet.Web.Endpoint.GetWallets
    ( getWalletsIO
    ) where

import           Universum

import qualified Data.Text.IO                      as TIO

import           Client.Pos.Wallet.Web.Api         (getWallets)
import           Client.Pos.Wallet.Web.Run         (runEndpointClient)
import           Bench.Pos.Wallet.Config.Endpoints (extractEndpointConfigFor)
import           Bench.Pos.Wallet.Types            (BenchEndpoint (..), CompleteConfig (..),
                                                    EndpointConfig (..),
                                                    Response, ResponseReport (..))

import           Pos.Wallet.Web.ClientTypes        (CWallet (..))

-- | Run 'GetWallets' client. As a result we will get a list of wallets.
getWalletsIO :: CompleteConfig -> IO ()
getWalletsIO conf@CompleteConfig {..} = do
    response <- runEndpointClient conf getWallets
    when needResponseAnalysis $ do
        let ResponseReport report = analyze response
        case extractEndpointConfigFor GetWalletsBench conf of
            Nothing -> return ()
            Just (EndpointConfig {..}) -> TIO.appendFile pathToResponseReports $ report <> "\n"
    return ()

-- | Analyze response with list of wallets.
analyze
    :: Response [CWallet]
    -> ResponseReport
analyze response =
    case response of
        Left problem ->
            ResponseReport $
                "Cannot get list of wallets: " <> problem
        Right (Left walletError) ->
            ResponseReport $
                "Server returned an error: " <> pretty walletError
        Right (Right listOfWallets) -> do
            ResponseReport $
                show listOfWallets
